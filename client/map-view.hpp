#ifndef _MAP_VIEW_HPP_INCLUDED
#define _MAP_VIEW_HPP_INCLUDED

#include <Qt>
#include <QGridLayout>
#include <QLabel>

#include "map-cell.hpp"
#include "map-model.hpp"

class MapView : public QFrame {
    Q_OBJECT
    
    public:
        /*
         * Passing a pointer to the model into the constructor for the view
         * defeats the entire purpose of model-view-controller development.
         * This is a TEMPORARY HACK; don't leave it in.
         * FIXME: Find a way to get information between model and view without
         * violating MVC.
         */
        MapView(unsigned int width, unsigned int height,
                const MapModel *model);
        ~MapView();
        
        void movePlayer(int delta_x, int delta_y);
        
        void updateCellAt(int x, int y);
        void updateAllCells();
    
    private:
        // FIXME: Should we mark this const? It doesn't modify the object, but
        // it makes it really easy for the caller to (though technically it's
        // not the object they're modifying...).
        QLabel * getImageWidgetAt(int x, int y);
        // int convertCoordinates(int x, int y) const;
    
    private:
        unsigned int width;
        unsigned int height;
        
        unsigned int player_x;
        unsigned int player_y;
        
        const MapModel *model;
        
        // QLabel **image_widgets;
        
        QGridLayout *layout;
};

#endif // _MAP_VIEW_HPP_INCLUDED
