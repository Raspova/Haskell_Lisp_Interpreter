BINARY_PATH 	:=	$(shell stack path --local-install-root)
NAME 			= 	hal

all:
	stack build
	cp "`stack path --local-install-root`/bin/Hal-exe" $(NAME)

clean:
	stack clean

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re