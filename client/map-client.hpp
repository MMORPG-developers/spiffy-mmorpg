#ifndef _MAP_CLIENT_HPP_INCLUDED
#define _MAP_CLIENT_HPP_INCLUDED

#include <Qt>
#include <QFrame>
#include <QHBoxLayout>
#include <QKeyEvent>

#include "map-view.hpp"
#include "map-model.hpp"

class MapClient : public QFrame {
    Q_OBJECT
    
    public:
        MapClient(unsigned int width, unsigned int height);
        ~MapClient();
    
    protected:
        virtual void keyPressEvent(QKeyEvent *event);
    
    private:
        void sendWalkCommand(QString direction);
    
    signals:
        void sendServerCommand(QString command);
    
    public slots:
        void handleServerPacket(QString packet);
    
    private:
        MapModel *model;
        MapView *view;
        
        // Could just as well be any other type of layout for now...
        QHBoxLayout *layout;
};

#endif // _MAP_CLIENT_HPP_INCLUDED
