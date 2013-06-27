#include "map-model.hpp"

MapModel::MapModel()
{
    // The player always starts at (0, 0).
    player_x_ = 0;
    player_y_ = 0;
    
    // Look, this is a terrible solution. But we need to stop using a static
    // array anyway, so whatever. Until then, we'll choose these magic values
    // because they happen to work with the current server room size.
    // FIXME: Magic numbers
    origin_x_ = 5;
    origin_y_ = 5;
    
    outside_cell_ = UNKNOWN;
    
    // Until we're told otherwise, we know nothing about any cell.
    for (int i=0; i<25*25; ++i) {
        cells[i] = UNKNOWN;
    }
}

MapModel::~MapModel()
{
    // Nothing to clean up, yet.
}

const MapCell & MapModel::getCellAt(int x, int y) const
{
    int index = convertCoordinates(x, y);
    
    // If we have information about that cell, return it.
    // Otherwise, give the same information we give for all out-of-bounds
    // cells, namely "I have no idea what's there".
    if (index >= 0) {
        return cells[index];
    }
    else {
        return outside_cell_;
    }
}

void MapModel::setCellAt(int x, int y, const MapCell & new_cell)
{
    int index = convertCoordinates(x, y);
    
    // If we have a spot in our array for the given coordinates, use it and
    // save the new cell information.
    // If not, then for now just crash because we're using a static array.
    if (index >= 0) {
        cells[index] = new_cell;
        
        // Inform anyone interested that the cell was updated.
        emit cellUpdated(x, y);
    }
    else {
        // FIXME: Don't throw strings.
        throw "Invalid cell coordinates";
    }
}

void MapModel::movePlayer(int delta_x, int delta_y)
{
    // Change the player's location.
    player_x_ += delta_x;
    player_y_ += delta_y;
    
    // Inform anyone interested that the player moved.
    emit playerMoved(delta_x, delta_y);
}

int MapModel::convertCoordinates(int x, int y) const
{
    // Convert the given world coordinates to be relative to our static array.
    int array_x = origin_x_ + x;
    int array_y = origin_y_ + y;
    
    // Check that they're in bounds. If not, return -1.
    if (array_x < 0 || array_x >= 25) {
        return -1;
    }
    if (array_y < 0 || array_y >= 25) {
        return -1;
    }
    
    // Turn 2-dimensional coordinates into a 1-dimensional coordinate.
    return array_y * 25 + array_x;
}

