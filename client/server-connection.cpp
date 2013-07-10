#include "server-connection.hpp"

#include <iostream>
#include <string>

/*
 * The number of bytes in a packet header.
 * DO NOT CHANGE THIS unless you also change it in the server.
 */
static const int HEADER_SIZE = 2;

ServerConnection::ServerConnection(const QString &server, quint16 port)
{
    socket_ = new QTcpSocket(this);
    
    // Connect to the server immediately.
    socket_->connectToHost(server, port);
    
    // Start listening for packets from the server.
    QObject::connect(socket_, SIGNAL(readyRead()),
                     this, SLOT(socketReadyToRead()));
}

ServerConnection::~ServerConnection()
{
    // Calling socket_->disconnectFromHost() will try to write all remaining
    // data to the socket before closing it. Calling abort() will immediately
    // close it. I don't think disconnectFromHost() blocks until the socket is
    // disconnected, so if we used it we'd have to manually wait until the
    // socket is disconnected.
    // I'm not sure which one is the better idea, but calling
    // disconnectFromHost() in a destructor seems a little unwise (it could
    // potentially take a while).
    socket_->abort();
    // socket_->disconnectFromHost();
    
    // I think Qt already cleans up children on destruction, so we shouldn't
    // need to delete socket_.
    // But on the offchance we do, and if we use disconnectFromHost() above,
    // we should make sure to wait until the socket is finished disconnecting
    // before deleting socket_.
}

void ServerConnection::writeAll(const QByteArray &data)
{
    // Get the data array and size.
    const char *bytes = data.data();
    int len = data.size();
    
    while (len > 0) {
        // Try to write some more.
        int bytes_written = socket_->write(bytes, len);
        
        // If we fail, stop and signal an error.
        if (bytes_written < 0) {
            QString error_message("Write failed: ");
            error_message.append(socket_->errorString());
            emit error(error_message);
            return;
        }
        
        // Otherwise, there's that much less to write.
        bytes += bytes_written;
        len -= bytes_written;
    }
}

QByteArray ServerConnection::readWithExactSize(qint64 size)
{
    QByteArray data;
    
    while (data.size() < size) {
        // Get more data, up to the amount we want to read.
        QByteArray next_data = socket_->read(size - data.size());
        data.append(next_data);
    }
    
    if (data.size() != size) {
        // We always limit our read size to the desired amount, and we don't
        // stop reading until we have enough, so this shouldn't happen unless
        // the caller passes a negative size or something terrible happens.
        emit error("Failed to read correct amount of data.");
    }
    
    return data;
}

void ServerConnection::sendText(QString text)
{
    // Convert text from a QString to a QByteArray.
    QByteArray data;
    data.append(text);
    
    size_t len = data.size();
    
    // Calculate the maximum packet size, represented in binary as HEADER_SIZE
    // bytes of ones.
    // (This is the largest number that can be represented as an unsigned
    // integer of HEADER_SIZE bytes.)
    // 
    // A size_t should be big enough to store the size of any array we can
    // allocate on a given machine, so if we overflow our type and
    // underestimate the maximum packet size that the protocol can support,
    // it's probably just as well because we shouldn't be sending a packet
    // bigger than all of our memory anyway.
    size_t maximum_packet_size = 0;
    for (int i = 0; i < HEADER_SIZE; ++i) {
        maximum_packet_size <<= 8;
        maximum_packet_size |= 0xff;
    }
    
    // Check that we haven't exceeded the maximum packet size.
    if (len >= maximum_packet_size) {
        emit error(QString("Too much data to send at once."));
        return;
    }
    
    std::cerr << "[sent] " << text.toStdString() << std::endl;
    
    char header_buff[HEADER_SIZE];
    
    // Encode the size in HEADER_SIZE bytes by putting the bytes into the
    // header one at a time.
    for (int i = 1; i <= HEADER_SIZE; ++i) {
        header_buff[HEADER_SIZE - i] = (len & 0xff);
        len >>= 8;
    }
    
    // Write the header and the packet to the socket.
    QByteArray header(header_buff, HEADER_SIZE);
    writeAll(header);
    writeAll(data);
}

void ServerConnection::socketReadyToRead()
{
    // Use a loop because we want to read as many packets as are in the buffer.
    // FIXME: Put a useful condition on this loop?
    while (true) {
        // If there isn't even a header's worth of bytes to read, stop right
        // now.
        QByteArray header = socket_->peek(HEADER_SIZE);
        if (header.size() != HEADER_SIZE) {
            return;
        }
        
        size_t packet_size = 0;
        
        // Decode the packet size.
        for (int i = 0; i < HEADER_SIZE; ++i) {
            int nextChar = header.at(i);
            packet_size <<= 8;
            packet_size |= (nextChar & 0xff);
        }
        
        // Only continue if the entire packet is available.
        if ((unsigned) socket_->bytesAvailable() < packet_size + HEADER_SIZE) {
            return;
        }
        
        // Skip past the header.
        readWithExactSize(HEADER_SIZE);
        
        // Read the packet and signal its arrival.
        QByteArray data = readWithExactSize(packet_size);
        QString s(data);
        
        std::cerr << "[received] " << s.toStdString() << std::endl;
        
        emit textArrived(s);
    }
}

