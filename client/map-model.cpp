#include "map-model.hpp"

MapModel::MapModel()
{
    // Look, this is a terrible solution. But we need to stop using a static
    // array anyway, so whatever. Until then, we'll choose these magic values
    // because they happen to work with the current server room size.
    // FIXME: Magic numbers
    reference_x = 5;
    reference_y = 5;
    
    outside_cell = UNKNOWN;
    
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
        return outside_cell;
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
    }
    else {
        // FIXME: Don't throw strings.
        throw "Invalid cell coordinates";
    }
}

void MapModel::movePlayer(int delta_x, int delta_y)
{
    // Since we take reference_x and reference_y into account when we do cell
    // lookups, all we need to do to move the player is change those values.
    reference_x += delta_x;
    reference_y += delta_y;
}

int MapModel::convertCoordinates(int x, int y) const
{
    // The given coordinates are relative to the player.
    // Adjust them to be relative to our array.
    int absolute_x = reference_x + x;
    int absolute_y = reference_y + y;
    
    // Check that they're in bounds. If not, return -1.
    if (absolute_x < 0 || absolute_x >= 25) {
        return -1;
    }
    if (absolute_y < 0 || absolute_y >= 25) {
        return -1;
    }
    
    // Turn 2-dimensional coordinates into a 1-dimensional coordinate.
    return absolute_y * 25 + absolute_x;
}

