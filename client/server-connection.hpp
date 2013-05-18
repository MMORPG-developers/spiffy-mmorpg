#ifndef _SERVER_CONNECTION_HPP_INCLUDED
#define _SERVER_CONNECTION_HPP_INCLUDED

#include <Qt>
#include <QTcpSocket>
#include <QByteArray>
#include <QString>

class ServerConnection : public QObject {
    Q_OBJECT
    
    public:
        ServerConnection(const QString &server, quint16 port);
        ~ServerConnection();
    
    signals:
        void textArrived(QString text);
        void error(QString message);
    
    public slots:
        void sendText(QString text);
    
    private slots:
        void socketReadyToRead();
    
    private:
        void writeAll(const QByteArray &data);
        QByteArray readWithExactSize(qint64 size);
        
        QTcpSocket *socket;
};

#endif // _SERVER_CONNECTION_HPP_INCLUDED
