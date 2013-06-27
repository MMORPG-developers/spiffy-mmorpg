#ifndef _MAP_CLIENT_HPP_INCLUDED
#define _MAP_CLIENT_HPP_INCLUDED

#include <Qt>
#include <QFrame>
#include <QHBoxLayout>
#include <QKeyEvent>

#include "map-view.hpp"
#include "map-model.hpp"

/*
 * Top-level class for the client.
 * Handles key events, interprets packets from the server, and lays out all of
 * the client widgets (currently just the map).
 * Eventually, the job of interpreting packets from the server should probably
 * be delegated to another class.
 */
class MapClient : public QFrame {
    Q_OBJECT
    
    public:
        MapClient(unsigned int width, unsigned int height);
        ~MapClient();
    
    protected:
        // Override this in a subclass of QWidget to handle key events.
        virtual void keyPressEvent(QKeyEvent *event);
    
    private:
        /*
         * Trivial function to encode the string that indicates a "walk"
         * command and send it to the server.
         */
        void sendWalkCommand(QString direction);
    
    signals:
        /*
         * Emitted whenever this class wants to execute a server command.
         * Whatever class creates a MapClient should connect this signal to a
         * slot that will actually send the given string to the server.
         */
        void sendServerCommand(QString command);
    
    public slots:
        /*
         * Interpret a server command and handle it accordingly.
         * Whatever class creates a MapClient should connect this slot to a
         * signal that will be emitted whenever a packet arrives from the
         * server.
         */
        void handleServerPacket(QString packet);
    
    private:
        MapModel *model_;
        MapView *view_;
        
        // Could just as well be any other type of layout for now; there's only
        // one thing being laid out.
        QHBoxLayout *layout_;
};

#endif // _MAP_CLIENT_HPP_INCLUDED

