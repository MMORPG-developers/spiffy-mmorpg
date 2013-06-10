#include "map-client.hpp"

// #include <iostream>

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
    // std::cerr << "Packet from server: '" << packet.toStdString() << "'" << std::endl;
    
    // FIXME: Write and use helper functions.
    
    QRegExp space("\\s+");
    
    QStringList words = packet.split(space, QString::SkipEmptyParts);
    
    if (words[0] == "update_map_cell") {
        // FIXME: Be more robust.
        
        int x = words[1].toInt();
        int y = words[2].toInt();
        int is_wall = words[3].toInt();
        
        MapCell cell = is_wall? WALL : FLOOR;
        
        model->setCellAt(x, y, cell);
        view->updateCellAt(x, y);
    }
    else if (words[0] == "move_in_map") {
        throw "Not implemented yet....";
    }
    else {
        // FIXME: What did I tell you about throwing strings?
        throw "Unrecognized command from server";
    }
}

