const int IN_FD = 0;
const int OUT_FD = 1;

typedef unsigned char byte;

int read_cmd(byte *buf)
{
    int len;
    
    // Try to read the header.
    if (read_exact(buf, 2) != 2) {
        return -1;
    }
    
    // Translate the header into the actual length.
    len = (buf[0] << 8) | buf[1];
    
    // Read the actual message.
    return read_exact(buf, len);
}

int write_cmd(byte *buf, int len)
{
    byte header_byte;
    
    // Make sure the message isn't too long.
    if (len > 0xffff) {
        return -1;
    }
    
    // Write the header, one byte at a time.
    // In real code, we'd probably use something more like what the client
    // currently does.
    header_byte = (len >> 8) & 0xff;
    write_exact(&header_byte, 1);
    header_byte = len & 0xff;
    write_exact(&header_byte, 1);
    
    return write_exact(buf, len);
}

int read_exact(byte *buf, int len)
{
    int i;
    int got = 0;
    
    do {
        i = read(IN_FD, buf+got, len-got);
        if (i <= 0) {
            return i;
        }
        got += i;
    } while (got < len);
    
    return len;
}

int write_exact(byte *buf, int len)
{
    int i;
    int wrote = 0;
    
    do {
        i = write(OUT_FD, buf+wrote, len-wrote);
        if (i <= 0) {
            return i;
        }
        wrote += i;
    } while (wrote < len);
    
    return len;
}


int foo(int x) {
    return x + 1;
}

int bar(int x) {
    return 2 * x;
}


int main()
{
    int func, arg, ret;
    // This is a huge buffer.
    // The example suggested using a buffer of size 100.
    // In this particular example, that should be fine.
    // But if we're going to use 2-byte headers and not check the lengths,
    // then I'm not going to blindly stuff data into a 100-byte buffer.
    // I didn't do the buffer overflow lab in CS 105 for nothing.
    // Probably in actuality, we want to pass a maximum buffer size to the
    // read_cmd function and have it crash appropriately if the Erlang tries to
    // overflow the buffer.
    // Erm... or just allocate a larger buffer. Assuming we can free the memory
    // once we're done.
    byte buf[0x10000];
    
    while (read_cmd(buf) > 0) {
        func = buf[0];
        arg = buf[1];
        
        if (func == 1) {
            ret = foo(arg);
        }
        else if (func == 2) {
            ret = bar(arg);
        }
        // else {???}
        
        // Excuse me? What if the result is more than one byte?
        // In real code, we would calculate the length a little more carefully.
        buf[0] = ret;
        write_cmd(buf, 1);
    }
}

