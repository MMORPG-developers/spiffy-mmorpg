#include "map-client.hpp"

#include <QRegExp>
#include <QString>
#include <QStringList>

#include "map-cell.hpp"

MapClient::MapClient(unsigned int width, unsigned int height)
{
    model = new MapModel();
    view = new MapView(width, height, model, this);
    
    layout = new QHBoxLayout(this);
    
    layout->addWidget(view);
    setLayout(layout);
}

MapClient::~MapClient()
{
    delete model;
    // delete view;
}

void MapClient::handleServerPacket(QString packet)
{
    QRegExp space("\\s+");
    
    QStringList words = packet.split(space, QString::SkipEmptyParts);
    
    if (words[0] == "update_map_cell") {
        // FIXME: Be more robust.
        
        int x = words[1].toInt();
        int y = words[2].toInt();
        int is_wall = words[3].toInt();
        
        MapCell cell = is_wall? WALL : FLOOR;
        
        model->setCellAt(x, y, cell);
        view->updateRelativeCell(x, y);
    }
    else if (words[0] == "move_in_map") {
        int delta_x = words[1].toInt();
        int delta_y = words[2].toInt();
        
        model->movePlayer(delta_x, delta_y);
        view->movePlayer(delta_x, delta_y);
    }
    else {
        // FIXME: Don't throw strings.
        throw "Unrecognized command from server";
    }
}

void MapClient::keyPressEvent(QKeyEvent *event)
{
    switch (event->key()) {
        case Qt::Key_H:
            sendWalkCommand("west");
            break;
        
        case Qt::Key_J:
            sendWalkCommand("south");
            break;
        
        case Qt::Key_K:
            sendWalkCommand("north");
            break;
        
        case Qt::Key_L:
            sendWalkCommand("east");
            break;
        
        case Qt::Key_Y:
            sendWalkCommand("northwest");
            break;
        
        case Qt::Key_U:
            sendWalkCommand("northeast");
            break;
        
        case Qt::Key_B:
            sendWalkCommand("southwest");
            break;
        
        case Qt::Key_N:
            sendWalkCommand("southeast");
            break;
        
        default:
            QFrame::keyPressEvent(event);
            break;
    }
}

void MapClient::sendWalkCommand(QString direction)
{
    QString command = "action walk " + direction;
    emit sendServerCommand(command);
}

