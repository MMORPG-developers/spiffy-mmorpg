#include "serverconnection.hpp"

#include <string>

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

void ServerConnection::sendText(QString text)
{
    int len = text.size();
    
    // TODO: There must be an easier way to do this...
    // preferably without having to specify a character encoding.
    std::string as_std_string = text.toStdString();
    const char *s = as_std_string.c_str();
    
    while (len > 0) {
        int bytes_written = socket->write(s, len);
        
        if (bytes_written < 0) {
            // FIXME: Please please please please PLEASE use a real exception.
            throw "Write failed.";
        }
        
        s += bytes_written;
        len -= bytes_written;
    }
}

void ServerConnection::socketReadyToRead()
{
    QByteArray data = socket->readAll();
    QString s(data);
    
    emit textArrived(s);
}

