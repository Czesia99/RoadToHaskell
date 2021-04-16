##
## EPITECH PROJECT, 2021
## 02-day2
## File description:
## Makefile
##

CC = ghc
SRC = DoOp.hs
NAME = doop
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