##
## EPITECH PROJECT, 2021
## wolfram
## File description:
## Makefile
##

CC = ghc
SRC = wolfram.hs
NAME = wolfram
OBJ = *.o *.hi
RM = rm -f

all: $(NAME)

$(NAME):
	$(CC) $(SRC) -o $(NAME)

clean:
	$(RM) $(OBJ)

fclean:
	$(RM) $(NAME)

re: fclean all