#include <iostream>

#include "snet-client.hpp"
#include "server-connection.hpp"

#include "map-client.hpp"

#include <QApplication>
#include <QObject>
#include <QString>

// const char *SERVER = "crossfire.metalforge.net";
const char *SERVER = "localhost";
const int PORT = 6667;

int main(int argc, char **argv)
{
    try {
        QApplication app(argc, argv);
        
        MapClient widget(25, 25);
        
        ServerConnection connection(SERVER, PORT);
        
        QObject::connect(&widget, SIGNAL(sendServerCommand(QString)),
                         &connection, SLOT(sendText(QString)));
        QObject::connect(&connection, SIGNAL(textArrived(QString)),
                         &widget, SLOT(handleServerPacket(QString)));
        
     // // FIXME: This is a hack. But at least error messages get printed
     // // *somewhere* this way, instead of simply vanishing.
     // QObject::connect(&connection, SIGNAL(error(QString)),
     //                  &widget, SLOT(appendText(QString)));
        
        // TODO: Add a handler for when the user closes the window.
        
        // TODO: Add a handler for errors ocurring (this should be handled by a
        // top-level Client class, which will need to be a Qt object). This will
        // replace the above connection between connection.error and
        // widget.appendText.
        
        widget.show();
        
        return app.exec();
    }
    catch (const char *s) {
        std::cerr << "Error: " << s << std::endl;
    }
}

