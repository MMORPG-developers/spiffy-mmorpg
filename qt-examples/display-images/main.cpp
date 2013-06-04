#include <QApplication>
// #include <QObject>
#include <QPixmap>
#include <QLabel>
#include <QGridLayout>

const char *TL_IMAGE_FILE = "images/lena.jpg";
const char *TR_IMAGE_FILE = "images/wide.png";
const char *BL_IMAGE_FILE = "images/text.svg";
const char *BR_IMAGE_FILE = "images/xkcd.png";

int main(int argc, char **argv)
{
    QApplication app(argc, argv);
    
    // Widget that will be the window.
    QFrame frame;
    
    // Load the images into QPixmaps.
    QPixmap tl_image(TL_IMAGE_FILE);
    QPixmap tr_image(TR_IMAGE_FILE);
    QPixmap bl_image(BL_IMAGE_FILE);
    QPixmap br_image(BR_IMAGE_FILE);
    
    // QPixmaps aren't widgets, so wrap them in QLabels.
    QLabel tl_widget(&frame);
    QLabel tr_widget(&frame);
    QLabel bl_widget(&frame);
    QLabel br_widget(&frame);
    
    tl_widget.setPixmap(tl_image);
    tr_widget.setPixmap(tr_image);
    bl_widget.setPixmap(bl_image);
    br_widget.setPixmap(br_image);
    
    // Scale some of them.
    tl_widget.setScaledContents(true);
 // tr_widget.setScaledContents(true);
 // bl_widget.setScaledContents(true);
 // br_widget.setScaledContents(true);
    
    // Put them all in a layout manager so they can be displayed in a grid.
    QGridLayout layout(&frame);
    
    layout.addWidget(&tl_widget, 0, 0);
    layout.addWidget(&tr_widget, 0, 1);
    layout.addWidget(&bl_widget, 1, 0);
    layout.addWidget(&br_widget, 1, 1);
    
    frame.setLayout(&layout);
    
    // Actually show the window.
    frame.show();
    
    return app.exec();
}

