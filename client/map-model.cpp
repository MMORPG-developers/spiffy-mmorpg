#include "map-model.hpp"

MapModel::MapModel()
{
    // FIXME: Magic numbers
    reference_x = 5;
    reference_y = 5;
    
    outside_cell = UNKNOWN;
    
    for (int i=0; i<25*25; ++i) {
        cells[i] = UNKNOWN;
    }
}

MapModel::~MapModel()
{
    // ...
}

const MapCell & MapModel::getCellAt(int x, int y) const
{
    int index = convertCoordinates(x, y);
    
    if (index >= 0) {
        return cells[index];
    }
    else {
        return outside_cell;
    }
}

void MapModel::setCellAt(int x, int y, const MapCell & new_cell)
{
    int index = convertCoordinates(x, y);
    
    if (index >= 0) {
        cells[index] = new_cell;
    }
    else {
        // FIXME: Don't throw strings.
        throw "Invalid cell coordinates";
    }
}

void MapModel::movePlayer(int delta_x, int delta_y)
{
    reference_x += delta_x;
    reference_y += delta_y;
}

int MapModel::convertCoordinates(int x, int y) const
{
    int absolute_x = reference_x + x;
    int absolute_y = reference_y + y;
    
    if (absolute_x < 0 || absolute_x >= 25) {
        return -1;
    }
    if (absolute_y < 0 || absolute_y >= 25) {
        return -1;
    }
    
    return absolute_y * 25 + absolute_x;
}

