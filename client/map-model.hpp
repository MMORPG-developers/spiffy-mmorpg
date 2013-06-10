#ifndef _MAP_MODEL_HPP_INCLUDED
#define _MAP_MODEL_HPP_INCLUDED

#include "map-cell.hpp"

/*
 * Class to store all known information about the map.
 */
class MapModel {
    public:
        MapModel();
        ~MapModel();
        
        /*
         * For both of the following, coordinates are given relative to the
         * player's current location.
         * 
         * getCellAt is only guaranteed to return a reference/pointer to a
         * MapCell with the correct data. Subsequent calls to getCellAt at the
         * same location might return new pointers and calls to getCellAt at a
         * different location might return the same pointer (assuming the two
         * cells look the same -- for example, if nothing is known about either
         * cell).
         */
        const MapCell & getCellAt(int x, int y) const;
        void setCellAt(int x, int y, const MapCell &new_cell);
        
        /*
         * Move the player by the given amount. All future calls to getCellAt
         * and setCellAt will use coordinates relative to the new player
         * location.
         */
        void movePlayer(int delta_x, int delta_y);
    
    private:
        /*
         * Returns the index of cell (x, y) in the array cells.
         * If the coordinates are out of bounds, return -1.
         */
        int convertCoordinates(int x, int y) const;
    
    private:
        /*
         * The array of cells. It's a one-dimensional array, given a second
         * dimension through the convertCoordinates function.
         * FIXME: A static array is a terrible way of implementing this.
         * FIXME: Magic numbers
         */
        MapCell cells[25*25];
        
        /*
         * The player's current location. All coordinates are relative to this
         * location.
         */
        int reference_x;
        int reference_y;
        
        /*
         * Dummy cell. We return a reference to this if someone asks for a
         * reference to a cell out of bounds.
         */
        MapCell outside_cell;
};

#endif // _MAP_MODEL_HPP_INCLUDED

