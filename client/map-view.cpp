#include "map-view.hpp"

#include <QPixmap>

/*
 * Returns an image object for the given map cell.
 */
static QPixmap getImage(MapCell cell)
{
    QPixmap image;
    
    // Currently there are only four images and MapCell is an enum, so just
    // match the given cell to the correct image file.
    if (cell == UNKNOWN) {
        image = QPixmap("images/unknown.png");
    }
    else if (cell == FLOOR) {
        image = QPixmap("images/floor.png");
    }
    else if (cell == WALL) {
        image = QPixmap("images/wall.png");
    }
    else if (cell == PLAYER) {
        image = QPixmap("images/player.png");
    }
    
    return image;
}


MapView::MapView(unsigned int width, unsigned int height,
                 const MapModel *model, QWidget *parent)
    : QFrame(parent)
{
    // Fill in instance data.
    // FIXME: Use a colon initializer?
    width_ = width;
    height_ = height;
    
    player_world_x_ = 0;
    player_world_y_ = 0;
    
    // The player goes in the center.
    player_screen_x_ = width_/2;
    player_screen_y_ = height_/2;
    
    model_ = model;
    
    layout_ = new QGridLayout(this);
    
    // Put only very little space between tiles.
    layout_->setHorizontalSpacing(1);
    layout_->setVerticalSpacing(1);
    
    // Create all the image widgets and lay them out in the grid.
    for (unsigned int x = 0; x < width_; ++x) {
        for (unsigned int y = 0; y < height_; ++y) {
            QLabel *image_widget = new QLabel(this);
            layout_->addWidget(image_widget, y, x);
        }
    }
    
    // Get whatever data the model already has.
    updateAllCells();
}

MapView::~MapView()
{
    // I think Qt already cleans up children on destruction, so we shouldn't
    // need to delete layout.
}

void MapView::updateCell(int x, int y)
{
    QLabel *image_widget = getImageWidgetAt(x, y);
    
    // If the given coordinates are out of bounds, then do nothing.
    if (image_widget) {
        // Get the cell from the model.
        MapCell cell = model_->getCellAt(x, y);
        
        // Special case: Always draw the player at the player's location.
        // FIXME: This is the server's job, not ours.
     // if (x == player_world_x_ && y == player_world_y_) {
     //     cell = PLAYER;
     // }
        
        // Display the appropriate image at the appropriate location.
        QPixmap image = getImage(cell);
        image_widget->setPixmap(image);
    }
}

void MapView::updateAllCells()
{
    // For each cell: update it.
    for (unsigned int screen_x = 0; screen_x < width_; ++screen_x) {
        for (unsigned int screen_y = 0; screen_y < height_; ++screen_y) {
            // Convert the coordinates (the model needs world coordinates).
            int world_x = screenToWorldX(screen_x);
            int world_y = screenToWorldY(screen_y);
            
            updateCell(world_x, world_y);
        }
    }
}

void MapView::movePlayer(int delta_x, int delta_y)
{
    // Update the player's position.
    player_world_x_ += delta_x;
    player_world_y_ += delta_y;
    
    // Eventually, we should move cells over and only go to the model for
    // information on the ones that were just brought into the display region.
    // But for now, just redraw them all.
    updateAllCells();
}

QLabel * MapView::getImageWidgetAt(int x, int y)
{
    // Convert the given world coordinates to screen coordinates.
    int screen_x = worldToScreenX(x);
    int screen_y = worldToScreenY(y);
    
    // Check that they're in bounds. If not, return NULL.
    if (screen_x < 0 || (unsigned) screen_x >= width_) {
        return NULL;
    }
    if (screen_y < 0 || (unsigned) screen_y >= height_) {
        return NULL;
    }
    
    // Ask the grid layout for the widget at those coordinates.
    // Recast it to the type it's supposed to be.
    QLayoutItem *layout_item = layout_->itemAtPosition(screen_y, screen_x);
    QWidget *widget = layout_item->widget();
    return static_cast<QLabel *>(widget);
}


int MapView::worldToScreenX(int world_x) const
{
    return world_x + (player_screen_x_ - player_world_x_);
}

int MapView::worldToScreenY(int world_y) const
{
    return world_y + (player_screen_y_ - player_world_y_);
}

int MapView::screenToWorldX(int screen_x) const
{
    return screen_x + (player_world_x_ - player_screen_x_);
}

int MapView::screenToWorldY(int screen_y) const
{
    return screen_y + (player_world_y_ - player_screen_y_);
}

