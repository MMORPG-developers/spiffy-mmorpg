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
    // FIXME: Don't use the same names for the parameters as for the instance
    // variables.
    this->width = width;
    this->height = height;
    
    this->player_world_x = 0;
    this->player_world_y = 0;
    
    // The player goes in the center.
    this->player_screen_x = this->width/2;
    this->player_screen_y = this->height/2;
    
    this->model = model;
    
    layout = new QGridLayout(this);
    
    // Put only very little space between tiles.
    layout->setHorizontalSpacing(1);
    layout->setVerticalSpacing(1);
    
    // Create all the image widgets and lay them out in the grid.
    for (unsigned int x = 0; x < this->width; ++x) {
        for (unsigned int y = 0; y < this->height; ++y) {
            QLabel *image_widget = new QLabel(this);
            layout->addWidget(image_widget, y, x);
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
        MapCell cell = model->getCellAt(x, y);
        
        // Special case: Always draw the player at the player's location.
        // FIXME: This is the server's job, not ours.
        if (x == player_world_x && y == player_world_y) {
            cell = PLAYER;
        }
        
        // Display the appropriate image at the appropriate location.
        QPixmap image = getImage(cell);
        image_widget->setPixmap(image);
    }
}

void MapView::updateAllCells()
{
    // For each cell: update it.
    for (unsigned int screen_x = 0; screen_x < width; ++screen_x) {
        for (unsigned int screen_y = 0; screen_y < height; ++screen_y) {
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
    player_world_x += delta_x;
    player_world_y += delta_y;
    
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
    if (screen_x < 0 || (unsigned) screen_x >= width) {
        return NULL;
    }
    if (screen_y < 0 || (unsigned) screen_y >= height) {
        return NULL;
    }
    
    // Ask the grid layout for the widget at those coordinates.
    // Recast it to the type it's supposed to be.
    QLayoutItem *layout_item = layout->itemAtPosition(screen_y, screen_x);
    QWidget *widget = layout_item->widget();
    return static_cast<QLabel *>(widget);
}


int MapView::worldToScreenX(int world_x) const
{
    return world_x + (player_screen_x - player_world_x);
}

int MapView::worldToScreenY(int world_y) const
{
    return world_y + (player_screen_y - player_world_y);
}

int MapView::screenToWorldX(int screen_x) const
{
    return screen_x + (player_world_x - player_screen_x);
}

int MapView::screenToWorldY(int screen_y) const
{
    return screen_y + (player_world_y - player_screen_y);
}

