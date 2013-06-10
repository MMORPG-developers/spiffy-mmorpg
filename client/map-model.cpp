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
    // FIXME: Don't use try/catch like this. Please. It's terrible.
    try {
        int index = convertCoordinates(x, y);
        
        return cells[index];
    }
    catch (...) {
        return outside_cell;
    }
}

void MapModel::setCellAt(int x, int y, const MapCell & new_cell)
{
    int index = convertCoordinates(x, y);
    
    cells[index] = new_cell;
}

int MapModel::convertCoordinates(int x, int y) const
{
    int absolute_x = reference_x + x;
    int absolute_y = reference_y + y;
    
    // FIXME: Don't throw strings.
    if (absolute_x < 0 || absolute_x >= 25) {
        throw "x index out of bounds";
    }
    if (absolute_y < 0 || absolute_y >= 25) {
        throw "y index out of bounds";
    }
    
    return absolute_y * 25 + absolute_x;
}

