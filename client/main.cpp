#include <iostream>
#include <string>

#include "network.hpp"

using namespace std;

const char *SERVER = "localhost";
// The port is specified as a string containing its decimal representation, not
// as the number itself. Presumably because the port needs to be sent as a
// string over the network anyway, but still... seriously guys?
const char *PORT = "6667";

int main(void)
{
    try {
        Socket sock(SERVER, PORT);
        sock.send("Why hello there, Mr. server!\n");
        string response = sock.receive(4096);
        
        cout << "Received \"\"\""
             << response
             << "\"\"\" from server."
             << endl;
        
        cout << "Yay, server/client communications work!" << endl;
    }
    catch (SystemCallException *err) {
        cerr << err->what() << endl;
    }
    
    return 0;
}

