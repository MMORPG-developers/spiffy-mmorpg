#include "server-connection.hpp"

#include <string>

/*
 * The number of bytes in a packet header.
 * DO NOT CHANGE THIS unless you also change it in the server.
 */
static const int HEADER_SIZE = 2;

ServerConnection::ServerConnection(const QString &server, quint16 port)
{
    socket = new QTcpSocket(this);
    
    socket->connectToHost(server, port);
    
    QObject::connect(socket, SIGNAL(readyRead()),
                     this, SLOT(socketReadyToRead()));
}

ServerConnection::~ServerConnection()
{
    socket->waitForDisconnected(-1);
    socket->close();
    
    delete socket;
}

void ServerConnection::writeAll(const QByteArray &data)
{
    int len = data.size();
    
    const char *bytes = data.data();
    
    while (len > 0) {
        int bytes_written = socket->write(bytes, len);
        
        if (bytes_written < 0) {
            // TODO: Get an explanatory error message, as from perror().
            emit error(QString("Write failed."));
            return;
        }
        
        bytes += bytes_written;
        len -= bytes_written;
    }
}

QByteArray ServerConnection::readWithExactSize(qint64 size)
{
    QByteArray data;
    
    while (data.size() < size) {
        QByteArray next_data = socket->read(size - data.size());
        data.append(next_data);
    }
    
    if (data.size() != size) {
        // This should never happen
        emit error("Failed to read correct amount of data.");
    }
    
    return data;
}

void ServerConnection::sendText(QString text)
{
    // QByteArray data = text.toLocal8Bit();
    
    // Apparently there isn't a constructor that does this?
    QByteArray data;
    data.append(text);
    
    int len = data.size();
    
    if (len > 0xffff) {
        emit error(QString("Too much data to send at once."));
        return;
    }
    
    char header_buff[HEADER_SIZE];
    
    for (int i = 0; i < HEADER_SIZE; ++i) {
        header_buff[i] = (len >> (8 * (HEADER_SIZE - 1 - i))) & 0xff;
    }
    
    QByteArray header(header_buff, HEADER_SIZE);
    
    writeAll(header);
    writeAll(data);
}

void ServerConnection::socketReadyToRead()
{
    // TODO: Finite loop?
    while (true) {
        QByteArray header = socket->peek(HEADER_SIZE);
        if (header.size() != HEADER_SIZE) {
            return;
        }
        
        int packetSize = 0;
        
        for (int i = 0; i < HEADER_SIZE; ++i) {
            int nextChar = header.at(i);
            packetSize |= (nextChar & 0xff) << (8 * (HEADER_SIZE - 1 - i));
        }
        
        if (socket->bytesAvailable() < packetSize + HEADER_SIZE) {
            return;
        }
        
        // Skip past the header.
        readWithExactSize(HEADER_SIZE);
        
        // Read and signal the packet.
        QByteArray data = readWithExactSize(packetSize);
        QString s(data);
        
        emit textArrived(s);
    }
}
