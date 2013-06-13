all: main.beam

clean:
	rm *.beam

.phony: all clean

%.beam: %.erl
	erlc $<



# Currently this list is sorted such that the dependencies for each file all
# occur below it. As long as we avoid circular dependencies (and this is
# therefore possible), let's keep things this way.

# FIXME: I'm not positive these are all the dependencies. I'm not even sure I
# know how to generate the correct set of dependencies. Once we figure that
# out, we might want to write a program to generate the dependency list.

# FIXME: Actually, an alternative solution is to just get a list of all .erl
# files and erlc any that need to be erlc'd. I mean, it'd be easy and I'm
# pretty sure it's actually what we want. The only real disadvantage is that
# it will also compile unused files.

main.beam: info_manager.beam player_control.beam \
	map.beam user_info_manager.beam tag.beam map_cell.hrl user_info.hrl

info_manager.beam: map.beam user_info_manager.beam map_cell.hrl

player_control.beam: client_connection.beam map_cell.hrl

map.beam: user_info_manager.beam array_2d.beam map_cell.hrl

user_info_manager.beam: user_info.hrl

# tag.beam:

# array_2d.beam:

# client_connection.beam:

