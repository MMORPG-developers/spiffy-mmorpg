#ifndef _MAP_MODEL_HPP_INCLUDED
#define _MAP_MODEL_HPP_INCLUDED

#include "map-cell.hpp"

class MapModel {
    public:
        MapModel();
        ~MapModel();
        
        /*
         * For both of the following, coordinates are given relative to the
         * player.
         */
        const MapCell & getCellAt(int x, int y) const;
        void setCellAt(int x, int y, const MapCell &new_cell);
        
        /*
         * Move the reference cell by the given amount. All future calls to
         * getCellAt and setCellAt will use coordinates relative to the new
         * reference cell.
         */
        void movePlayer(int delta_x, int delta_y);
    
    private:
        // FIXME: Magic numbers
        // FIXME: A static array is a terrible way of implementing this.
        MapCell cells[25*25];
        int reference_x;
        int reference_y;
        
        int convertCoordinates(int x, int y) const;
};

#endif // _MAP_MODEL_HPP_INCLUDED
