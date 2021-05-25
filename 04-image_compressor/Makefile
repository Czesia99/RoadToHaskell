##
## EPITECH PROJECT, 2021
## Image Compressor Haskell
## File description:
## Makefile
##

CC = stack build
NAME = imageCompressor
EXE = image-compressor-exe
FOLDER = image-compressor

all: $(NAME)

$(NAME):
	cd $(FOLDER) && $(CC) --copy-bins --local-bin-path ../
	mv $(EXE) $(NAME)

clean:
	cd $(FOLDER) && stack clean

fclean: clean
		rm $(NAME)

purge:	
	cd $(FOLDER) && stack purge
	rm $(NAME)

re: fclean all