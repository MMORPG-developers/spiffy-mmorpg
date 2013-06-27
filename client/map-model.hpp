#ifndef _MAP_MODEL_HPP_INCLUDED
#define _MAP_MODEL_HPP_INCLUDED

#include "map-cell.hpp"

#include <QObject>

/*
 * Class to store all known information about the map.
 */
class MapModel : public QObject {
    Q_OBJECT
    
    public:
        MapModel();
        ~MapModel();
        
        /*
         * For both of the following, coordinates are given relative to
         * wherever the player entered this map.
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
         * Move the player by the given amount.
         * Currently all this does is cause the playerMoved signal to be
         * emitted.
         */
        void movePlayer(int delta_x, int delta_y);
    
    signals:
        /*
         * Emitted whenever a cell changes (and therefore might need to be
         * redrawn).
         */
        void cellUpdated(int x, int y);
        
        /*
         * Emitted whenever the player moves.
         */
        void playerMoved(int delta_x, int delta_y);
    
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
         * The location of the cell (0, 0) in the static array. Unlike
         * everything else in the model, these are not world coordinates but
         * indices into the array.
         */
        int origin_x_;
        int origin_y_;
        
        /*
         * The player's current location (in world coordinates).
         */
        int player_x_;
        int player_y_;
        
        /*
         * Dummy cell. We return a reference to this if someone asks for a
         * reference to a cell out of bounds.
         */
        MapCell outside_cell_;
};

#endif // _MAP_MODEL_HPP_INCLUDED

