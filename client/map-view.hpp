#ifndef _MAP_VIEW_HPP_INCLUDED
#define _MAP_VIEW_HPP_INCLUDED

#include <Qt>
#include <QGridLayout>
#include <QLabel>

#include "map-cell.hpp"
#include "map-model.hpp"

/*
 * Class to display the map.
 */
class MapView : public QFrame {
    Q_OBJECT
    
    public:
        /*
         * Passing a pointer to the model into the constructor for the view
         * defeats the entire purpose of model-view-controller development.
         * I think.
         * Actually, I'm not sure I understand MVC....
         * Anyway, this is (might be?) a temporary hack; (maybe?) don't leave
         * it in.
         * 
         * FIXME: Figure out MVC; if this is inappropriate, then find a way to
         * get information between model and view without violating MVC.
         */
        MapView(unsigned int width, unsigned int height,
                const MapModel *model, QWidget *parent = NULL);
        ~MapView();
    
    public slots:
        /*
         * Fetch new information from the model for the map cell at the
         * specified coordinates.
         * Coordinates may point outside of the display region. This is not an
         * error. If this happens, the MapView is not obligated to do anything.
         */
        void updateCell(int x, int y);
        
        /*
         * Fetch new information from the model for all map cells.
         */
        void updateAllCells();
        
        /*
         * Move the player.
         * This might (currently always will) cause the entire map to be moved,
         * because the player needs to stay on the screen.
         */
        void movePlayer(int delta_x, int delta_y);
    
    private:
        /*
         * Returns a pointer to the Qt image-displaying widget corresponding to
         * world coordinates (x, y), or NULL if those coordinates point outside
         * the viewing region.
         * 
         * FIXME: Should we mark this const? It doesn't modify the object, but
         * it makes it really easy for the caller to (though technically it's
         * not the object they're modifying).
         */
        QLabel * getImageWidgetAt(int x, int y);
        
        /*
         * Convert between world and screen coordinates.
         * World coordinates are the coordinates used by all public functions
         * and the model. Screen coordinates are the coordinates of a tile
         * relative to the grid that the view displays.
         */
        int worldToScreenX(int world_x) const;
        int worldToScreenY(int world_y) const;
        int screenToWorldX(int screen_x) const;
        int screenToWorldY(int screen_y) const;
    
    private:
        /*
         * The size of the map.
         */
        unsigned int width;
        unsigned int height;
        
        /*
         * The player's current location, in both world and screen coordinates.
         * These two coordinate pairs are used for converting between screen
         * and world coordinates.
         */
        int player_world_x;
        int player_world_y;
        int player_screen_x;
        int player_screen_y;
        
        /*
         * The model, which is the ultimate authority (outside of the server,
         * anyway) on what we know about which map cells.
         */
        const MapModel *model;
        
        /*
         * Layout for the image widgets.
         */
        QGridLayout *layout;
};

#endif // _MAP_VIEW_HPP_INCLUDED

