#ifndef _SERVERCONNECTION_HPP_INCLUDED
#define _SERVERCONNECTION_HPP_INCLUDED

#include <Qt>
#include <QTcpSocket>
#include <QString>

class ServerConnection : public QObject {
    Q_OBJECT
    
    public:
        ServerConnection(const QString &server, quint16 port);
        ~ServerConnection();
    
    signals:
        void textArrived(QString text);
    
    public slots:
        void sendText(QString text);
    
    private slots:
        void socketReadyToRead();
    
    private:
        QTcpSocket *socket;
};

#endif // _SERVERCONNECTION_HPP_INCLUDED
