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
    
    // The player goes in the center.
    this->player_x = this->width/2;
    this->player_y = this->height/2;
    
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

void MapView::movePlayer(int delta_x, int delta_y)
{
    // Suppress compiler warnings.
    (void) delta_x;
    (void) delta_y;
    
    // Eventually, we should move cells over and only go to the model for
    // information on the ones that were just brought into the display region.
    // But for now, just redraw them all.
    updateAllCells();
}

void MapView::updateRelativeCell(int relative_x, int relative_y)
{
    QLabel *image_widget = getImageWidgetAt(relative_x, relative_y);
    
    // If the given coordinates are out of bounds, then do nothing.
    if (image_widget) {
        // Get the cell from the model.
        MapCell cell = model->getCellAt(relative_x, relative_y);
        
        // Special case: Always draw the player at the player's location.
        // FIXME: This is the server's job, not ours.
        if (relative_x == 0 && relative_y == 0) {
            cell = PLAYER;
        }
        
        // Display the appropriate image at the appropriate location.
        QPixmap image = getImage(cell);
        image_widget->setPixmap(image);
    }
}

void MapView::updateAbsoluteCell(int absolute_x, int absolute_y)
{
    // Adjust coordinates, thus reducing the problem to one already solved.
    updateRelativeCell(absolute_x - player_x, absolute_y - player_y);
}

void MapView::updateAllCells()
{
    // For each cell: update it.
    for (unsigned int x = 0; x < width; ++x) {
        for (unsigned int y = 0; y < height; ++y) {
            updateAbsoluteCell(x, y);
        }
    }
}

QLabel * MapView::getImageWidgetAt(int x, int y)
{
    // The given coordinates are relative to the player.
    // Adjust them to be relative to our array.
    int absolute_x = player_x + x;
    int absolute_y = player_y + y;
    
    // Check that they're in bounds. If not, return NULL.
    if (absolute_x < 0 || absolute_x >= (int) width) {
        return NULL;
    }
    if (absolute_y < 0 || absolute_y >= (int) height) {
        return NULL;
    }
    
    // Ask the grid layout for the widget at those coordinates.
    // Recast it to the type it's supposed to be.
    QLayoutItem *layout_item = layout->itemAtPosition(absolute_y, absolute_x);
    QWidget *widget = layout_item->widget();
    return static_cast<QLabel *>(widget);
}


