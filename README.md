# Social Media - Progetto

## Scaricare i tweet
Per scaricare i tweet utilizziamo la libreria `twint` di *Python* attraverso l'interfaccia [Anaconda](https://www.anaconda.com/products/individual); prima di tutto creiamo un nuovo ambiente, chiamato *twitter*, in cui installare le librerie necessarie, ovvero [twint](https://github.com/twintproject/twint) e [Pandas](https://pandas.pydata.org/).

Da *Anaconda Prompt (Anaconda3)*, possiamo creare l'ambiente col seguente comando:

```conda create -n twitter python=3.6```

Ora attiviamo l'ambiente e installiamo le librerie:

```conda activate twitter```

```conda install -c anaconda git```

```pip install --user --upgrade git+https://github.com/twintproject/twint.git@origin/master#egg=twint```

```pip install tornado==4.5.3```

```pip install ipykernel```

```ipython kernel install --user --name=twitter```

**Nota:** Gli ultimi due comandi permettono di aggiungere l'ambiente a *jupyter notebook*, ovvero sarà possibile creare dei notebook che utilizzano esso e non quello base.

Infine, per avviare *jupyter notebook* dobbiamo tornare all'ambiente base:

```conda deactivate```

```jupyter notebook```

**Nota:** Quando si attiva un ambiente, nel prompt si passa dalla scritta `(base)` a `(nome_ambiente)`; quando si disattiva, si ritorna a `(base)`.
