all: build install

NAME     := ld-preload-xcreatewindow
SRC      := $(NAME).c
HOOK     := $(NAME).so
HOOK_ABS := $(shell realpath $(HOOK))

build: $(HOOK)

$(HOOK): $(SRC)
	gcc $(SRC) -shared -lX11 -fPIC -o $(HOOK)

install:
	grep "$(NAME)" ~/.xprofile || echo "export LD_PRELOAD=$(HOOK_ABS)" >> ~/.xprofile

