#ifndef _SERVER_CONNECTION_HPP_INCLUDED
#define _SERVER_CONNECTION_HPP_INCLUDED

#include <Qt>
#include <QTcpSocket>
#include <QByteArray>
#include <QString>

/*
 * A wrapper around a socket that provides two advantages:
 *   1. Adds packet headers to outgoing packets and strips them from incoming
 *      packets.
 *   2. Provides a signal for receive and a slot for send, allowing it to be
 *      more seamlessly used with other classes' handlers.
 */
class ServerConnection : public QObject {
    Q_OBJECT
    
    public:
        ServerConnection(const QString &server, quint16 port);
        ~ServerConnection();
    
    signals:
        /*
         * Emitted whenever new data arrives from the server.
         * The passed string is always exactly one packet.
         */
        void textArrived(QString text);
        
        /*
         * Emitted whenever an error occurs.
         */
        void error(QString message);
    
    public slots:
        /*
         * Send the given packet to the server.
         * This class deals with putting any necessary packet headers on the
         * packet.
         */
        void sendText(QString text);
    
    private slots:
        /*
         * Used to detect when the socket is ready to read.
         */
        void socketReadyToRead();
    
    private:
        /*
         * Wrapper functions that read or write all of a string, retrying as
         * many times as are necessary.
         */
        void writeAll(const QByteArray &data);
        QByteArray readWithExactSize(qint64 size);
    
    private:
        QTcpSocket *socket;
};

#endif // _SERVER_CONNECTION_HPP_INCLUDED
