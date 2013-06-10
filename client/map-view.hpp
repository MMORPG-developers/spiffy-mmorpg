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
        
        /*
         * Move the player.
         * This might (currently always will) cause the entire map to be moved,
         * because the player needs to stay on the screen.
         */
        void movePlayer(int delta_x, int delta_y);
        
        /*
         * Fetch new information from the model for a map cell given relative
         * or absolute coordinates.
         * Coordinates may point outside of the display region. This is not an
         * error; the MapView will simply do nothing if that happens.
         */
        void updateRelativeCell(int relative_x, int relative_y);
        void updateAbsoluteCell(int absolute_x, int absolute_y);
        
        /*
         * Fetch new information from the model for all map cells.
         */
        void updateAllCells();
    
    private:
        /*
         * Returns a pointer to the Qt widget at coordinates (x, y)
         * (relative to the player) that we're using for displaying images.
         * 
         * FIXME: Should we mark this const? It doesn't modify the object, but
         * it makes it really easy for the caller to (though technically it's
         * not the object they're modifying).
         */
        QLabel * getImageWidgetAt(int x, int y);
    
    private:
        /*
         * The size of the map.
         */
        unsigned int width;
        unsigned int height;
        
        /*
         * The player's current location.
         */
        unsigned int player_x;
        unsigned int player_y;
        
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

