OCAMLOPTS=-thread unix.cmxa threads.cmxa

MLFILES=workqueue.ml
CMXFILES=$(patsubst %.ml,%.cmx,$(MLFILES))

.PHONY : test clean

test : workqueue
	./workqueue

workqueue : $(CMXFILES)
	ocamlopt -o $@ $(OCAMLOPTS) $^

%.cmx : %.ml
	ocamlopt -c $(OCAMLOPTS) $<

clean :
	rm -f *.cmi *.cmx *.o workqueue *~

