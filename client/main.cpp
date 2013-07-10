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
        
        // TODO: Add a handler for when the user closes the window.
        
        // TODO: Add a handler for errors ocurring (this should be handled by a
        // top-level Client class, which will need to be a Qt object).
        // Currently the signal ServerConnection::error is ignored.
        
        widget.show();
        
        return app.exec();
    }
    catch (const char *s) {
        // FIXME: Throw something more sophisticated than strings. Or use
        // signals and slots to broadcast error messages.
        // FIXME: Handle thrown exceptions somewhere other than main.
        std::cerr << "Error: " << s << std::endl;
    }
}

