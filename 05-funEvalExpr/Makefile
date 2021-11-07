##
## EPITECH PROJECT, 2021
## EvalExpr Haskell
## File description:
## Makefile
##

CC = stack build
NAME = funEvalExpr
EXE = funEvalExpr-exe

all: $(NAME)

$(NAME):
	$(CC) --copy-bins --local-bin-path .
	mv $(EXE) $(NAME)

clean:
	stack clean

fclean: clean
		rm $(NAME)

purge:	
	stack purge
	rm $(NAME)

tests run:
	stack test

re: fclean all