#include "map-view.hpp"

#include <QPixmap>

static QPixmap getImage(MapCell cell)
{
    QPixmap image;
    
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
                 const MapModel *model)
{
    // FIXME: Colon initializer?
    // FIXME: Name collision
    this->width = width;
    this->height = height;
    
    this->player_x = this->width/2;
    this->player_y = this->height/2;
    
    this->model = model;
    
    layout = new QGridLayout(this);
    
    layout->setHorizontalSpacing(1);
    layout->setVerticalSpacing(1);
    
    // image_widgets = new QLabel[this->width * this->height];
    
    for (unsigned int x = 0; x < this->width; ++x) {
        for (unsigned int y = 0; y < this->height; ++y) {
            QLabel *image_widget = new QLabel(this);
            layout->addWidget(image_widget, y, x);
        }
    }
    
    updateAllCells();
}

MapView::~MapView()
{
    // ...
    
    // FIXME: Clean up things?
    // delete [] image_widgets;
    // delete layout;
}

void MapView::movePlayer(int delta_x, int delta_y)
{
    player_x += delta_x;
    player_y += delta_y;
    
    updateAllCells();
}

void MapView::updateCellAt(int x, int y)
{
    QLabel *image_widget = getImageWidgetAt(x, y);
    MapCell cell = model->getCellAt(x, y);
    QPixmap image = getImage(cell);
    image_widget->setPixmap(image);
}

void MapView::updateAllCells()
{
    for (unsigned int x = 0; x < width; ++x) {
        for (unsigned int y = 0; y < height; ++y) {
            // FIXME: How the heck are we indexing?
            updateCellAt(x - player_x, y - player_y);
        }
    }
}

QLabel * MapView::getImageWidgetAt(int x, int y)
{
    int absolute_x = player_x + x;
    int absolute_y = player_y + y;
    
    // FIXME: Don't throw strings.
    if (absolute_x < 0 || absolute_x >= (int) width) {
        throw "x index out of bounds";
    }
    if (absolute_y < 0 || absolute_y >= (int) height) {
        throw "y index out of bounds";
    }
    
    QLayoutItem *layout_item = layout->itemAtPosition(absolute_y, absolute_x);
    QWidget *widget = layout_item->widget();
    return (QLabel *) widget;
    
    // return &(image_widgets[absolute_y * width + absolute_x]);
}


