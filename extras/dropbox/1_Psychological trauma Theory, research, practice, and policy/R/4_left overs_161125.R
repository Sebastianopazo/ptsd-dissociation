#####LEFT OVERS######



####### Proyect III: Test 2 moderational models:
#III.1: PDEQ_t0 (dissociation) moderates TQ_t0 (traumatic load) -> PCL_t1 (PTSD 1 month after)  
#III.2: PDEQ_t0 (dissociation) moderates TQ_t0 (traumatic load) -> BDI_t1 (depression 1 month after)

##BOLLOW ARE ONLY FAILED ATTEMPTS!!!!!!!!!!!

structure.diagram(fx, Phi=NULL,fy=NULL,labels=NULL,cut=.3,errors=FALSE,simple=TRUE,
                  regression=FALSE,lr=TRUE,Rx=NULL,Ry=NULL,digits=1,e.size=.1,
                  main="Structural model", ...)

structure.diagram(PDEQ_t0 ~ TQ_t0 ~ Edad, Phi=NULL,fy=NULL,labels=NULL,cut=.3,errors=FALSE,simple=TRUE,
                  regression=FALSE,lr=TRUE,Rx=NULL,Ry=NULL,digits=1,e.size=.1,
                  main="Structural model")


sem r tutorial   r bloggers.com
lavaan.diagram()


vars <- c("PDEQ_t0","TQ_t0", "Edad")
d.2 <- my.data[vars]
str(d.2)



fx <- fa(d.2)

structure.diagram(fa(d.2))

structure.diagram(fx, Phi=NULL,fy=NULL,labels=NULL,cut=.3,errors=FALSE,simple=TRUE,
                  regression=FALSE,lr=TRUE,Rx=NULL,Ry=NULL,digits=1,e.size=.1,
                  main="Structural model")



