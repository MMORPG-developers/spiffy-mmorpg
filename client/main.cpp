// #include <iostream>
// #include <string>

#include "snet-client.hpp"
#include "server-connection.hpp"

#include <QApplication>
#include <QObject>
#include <QString>

// using namespace std;

const char *SERVER = "crossfire.metalforge.net";
// const char *SERVER = "localhost";
const int PORT = 13327;

int main(int argc, char **argv)
{
    QApplication app(argc, argv);
    
    SNetClient widget;
    ServerConnection connection(SERVER, PORT);
    
    QObject::connect(&widget, SIGNAL(textSubmitted(QString)),
                     &connection, SLOT(sendText(QString)));
    QObject::connect(&connection, SIGNAL(textArrived(QString)),
                     &widget, SLOT(appendText(QString)));
    
    // TODO: Add a handler for when the user closes the window.
    // TODO: Add a handler for errors ocurring (this should be handled by a
    // top-level Client class, which will need to be a Qt object).
    
    widget.show();
    
    return app.exec();
}

