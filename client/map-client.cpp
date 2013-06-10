#include "map-client.hpp"

#include <iostream>

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
    std::cerr << "Packet from server: '" << packet.toStdString() << "'" << std::endl;
}

