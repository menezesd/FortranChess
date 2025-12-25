# Fortran compiler
FC = gfortran
FFLAGS = -O3 -Wall

# Source files
SRC = chess.f90 board_utils.f90 chess_types.f90 evaluation.f90 make_unmake.f90 move_generation.f90 search.f90 transposition_table.f90 user_input_processor.f90 move_ordering_heuristics.f90 game_state_checker.f90
OBJ = $(SRC:.f90=.o)

# Executable
EXE = chess

all: $(EXE)

$(EXE): $(OBJ)
	$(FC) $(FFLAGS) -o $@ $^

# Object file dependencies (based on which modules each file USEs)
OBJS = chess_types.o transposition_table.o board_utils.o move_generation.o evaluation.o make_unmake.o search.o user_input_processor.o move_ordering_heuristics.o game_state_checker.o chess.o
transposition_table.o: chess_types.o
board_utils.o: chess_types.o transposition_table.o
evaluation.o: chess_types.o board_utils.o
make_unmake.o: chess_types.o board_utils.o transposition_table.o
move_generation.o: chess_types.o board_utils.o
search.o: chess_types.o board_utils.o move_generation.o make_unmake.o evaluation.o transposition_table.o move_ordering_heuristics.o
user_input_processor.o: chess_types.o board_utils.o move_generation.o
move_ordering_heuristics.o: chess_types.o board_utils.o
game_state_checker.o: chess_types.o board_utils.o move_generation.o
chess.o: board_utils.o chess_types.o evaluation.o make_unmake.o move_generation.o search.o user_input_processor.o transposition_table.o game_state_checker.o

# Compile rule
%.o: %.f90
	$(FC) $(FFLAGS) -c $<

clean:
	rm -f *.o *.mod $(EXE)

