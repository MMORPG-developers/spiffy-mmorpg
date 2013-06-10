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
    
    // The MapView is the only thing displayed by the client.
    layout->addWidget(view);
    setLayout(layout);
}

MapClient::~MapClient()
{
    delete model;
    
    // I think Qt already cleans up children on destruction, so we shouldn't
    // need to delete view or layout.
}

void MapClient::handleServerPacket(QString packet)
{
    // Separate the "words" in the packet wherever there's whitespace.
    QRegExp space("\\s+");
    QStringList words = packet.split(space, QString::SkipEmptyParts);
    
    // See if we recognize the first word (the type of command/request).
    // FIXME: Be more robust.
    if (words[0] == "update_map_cell") {
        // New information about a map cell.
        
        // Extract the arguments and convert them to the correct types.
        int x = words[1].toInt();
        int y = words[2].toInt();
        int is_wall = words[3].toInt();
        
        // Create a MapCell from the given information.
        MapCell cell = is_wall? WALL : FLOOR;
        
        // Update the model and view.
        model->setCellAt(x, y, cell);
        view->updateRelativeCell(x, y);
    }
    else if (words[0] == "move_in_map") {
        // The player has moved.
        
        // Extract the arguments and convert them to the correct types.
        int delta_x = words[1].toInt();
        int delta_y = words[2].toInt();
        
        // Update the model and view.
        model->movePlayer(delta_x, delta_y);
        view->movePlayer(delta_x, delta_y);
    }
    else {
        // If we don't recognize the first word, it's an error.
        // FIXME: Don't throw strings.
        throw "Unrecognized command from server";
    }
}

void MapClient::keyPressEvent(QKeyEvent *event)
{
    // Currently, we only support 8 directional keys (HJKLYUBN).
    // Directions are as shown below (diagram copied with slight modification
    // from NetHack):
    
    /* 
     *      y k u
     *       \|/
     *      h- -l
     *       /|\
     *      b j n
     */
    
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
            // If we don't handle the key, then let someone else handle it.
            // Note: if MapClient is changed to inherit from a different class,
            // we must change this line as well to reflect the new parent
            // class.
            QFrame::keyPressEvent(event);
            break;
    }
}

void MapClient::sendWalkCommand(QString direction)
{
    // Format for a walk command is "action walk <direction>".
    QString command = "action walk " + direction;
    emit sendServerCommand(command);
}

