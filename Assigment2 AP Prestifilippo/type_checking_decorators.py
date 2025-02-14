import inspect
import types
from typing import Union

def _coerce_annotation_to_type(hint):
    """
    Se 'hint' è letteralmente None (come nell'annotazione -> None),
    lo converte in type(None). Altrimenti lo restituisce invariato.
    """
    if hint is None:
        return type(None)
    return hint

def _matches_type(value, hint):
    """
    Verifica se value rispetta il type hint 'hint'.
    Se non c'è annotazione, non controlla nulla. Converte None in type(None) se necessario.
    type(hint) is types.UnionType: Controlla se hint è una UnionType. In tal caso, Controlla se value è un'istanza 
    del tipo t, dove t è uno dei tipi forniti dall'Union, restituendo in tal caso true.
    Il controllo successivo controlla se hint coincide con typing.Union estraendo l'attributo Union dal tipo tramite __origin__ se presente. Poi effettua un controllo analogo al precedente.
    Infine verifica se value è istanza di uno dei tipi singpli passati.
    """
    if hint == inspect._empty:
        return True
    hint = _coerce_annotation_to_type(hint)
    # Caso: Python 3.10+ con "UnionType" (es. float|int)
    if type(hint) is types.UnionType:
        return any(isinstance(value, t) for t in hint.__args__)
    # Caso: typing.Union in Python < 3.10
    if getattr(hint, '__origin__', None) is Union:
        return any(isinstance(value, t) for t in hint.__args__)
    # Altrimenti ci aspettiamo che hint sia un singolo tipo,
    return isinstance(value, hint)


def print_types(func):
    """
    Stampa per ogni parametro il type hint formale e la classe dell'argomento
    effettivo, poi stampa il type hint di ritorno e il tipo effettivo del risultato.
    Viene definito un wrapper interno. 
    Si ottiene la firma della funzione func, inclusi i parametri e le annotazioni di tipo.
    Invoca la funzione originale con gli argomenti forniti e memorizza il risultato.
    Recupera i dettagli di ogni parametro, convertendoli in una lista.
    Stampa l'annotazione se presente e il nome di ciascun parametro
    Stampa il confronto tra il tipo di risultato previsto e il risultato attuale
    """
    def wrapper(*args, **kwargs):   
        sig = inspect.signature(func)
        params = list(sig.parameters.values())
        result = func(*args, **kwargs)
        for i, arg in enumerate(args):
            ann = params[i].annotation if i < len(params) else inspect._empty
            pname = params[i].name if i < len(params) else f'param_{i}'
            print(f"Formal par '{pname}':{ann}; actual par '{arg}':{type(arg)}")
        return_ann = sig.return_annotation
        print(f"Result type {return_ann}; actual result '{result}':{type(result)}")
        return result
    return wrapper


def type_check(func):
    """
    Stampa solo i mismatch di tipo (parametri e ritorno),
    ma non blocca la chiamata in nessun caso.
    Estrae i parametri della funzione. 
    Per ogni argomento, verifica se esiste un'annotazione di tipo (ann) e se è specificata.
    Se l'annotazione e l'argomento della funzione non hanno lo stesso tipo, viene stampato un messaggio.
    Viene invocata la funzione, e si controlla se il valore di ritorno ha stesso tipo del tipo atteso.
    """
    def wrapper(*args, **kwargs):
        sig = inspect.signature(func)
        params = list(sig.parameters.values())

        for i, arg in enumerate(args):
            if i < len(params):
                ann = params[i].annotation
                if ann != inspect._empty:
                    if not _matches_type(arg, ann):
                        print(f"Parameter '{i}' has value '{arg}', not of type '{ann}'")

        result = func(*args, **kwargs)

        return_ann = sig.return_annotation
        if return_ann != inspect._empty:
            if not _matches_type(result, return_ann):
                print(f"Result is '{result}', not of type '{return_ann}'")

        return result

    return wrapper


def bb_type_check(max_block):
    """
    Riceve il numero massimo di blocchi consentiti in caso di mismatch di tipo.
    decorator(func) prende la funzione da decorare. Utilizza una lista state per mantenere max_block mutabile all'interno del wrapper.
    Il wrapper(*args, **kwargs) avvolge la funzione originale func.
    Vengono fatti scorrere la lista di argomenti della funzione. Per ogni argomento, verifica se esiste un'annotazione di tipo.
    Se c'è un mismatch, aggiunge una tupla (indice, valore, tipo_atteso) alla lista mismatches.
    Se ci sono mismatch e state[0] > 0, stampa i mismatch e decrementa il counter di blocco e non invoca la funzione, uscendp.
    Altrimenti we ci sono mismatch ma state[0] <= 0, stampa i mismatch e chiama comunque la funzione decorata.
    Controlla il tipo di ritorno e stampa eventuali mismatch tra il tipo previsto e il tipo restituito.
  """
    def decorator(func):
        state = [max_block]

        def wrapper(*args, **kwargs):
            sig = inspect.signature(func)
            params = list(sig.parameters.values())

            mismatches = []
            for i, arg in enumerate(args):
                if i < len(params):
                    ann = params[i].annotation
                    if ann != inspect._empty:
                        if not _matches_type(arg, ann):
                            mismatches.append((i, arg, ann))

            if mismatches and state[0] > 0:
                for (i, val, ann) in mismatches:
                    print(f"Parameter '{i}' has value '{val}', not of type '{ann}'")
                state[0] -= 1
                print(f"Function blocked. Remaining blocks: {state[0]}")
                return None
            
            for (i, val, ann) in mismatches:
                print(f"Parameter '{i}' has value '{val}', not of type '{ann}'")

            result = func(*args, **kwargs)

            return_ann = sig.return_annotation
            if return_ann != inspect._empty:
                if not _matches_type(result, return_ann):
                    print(f"Result is '{result}', not of type '{return_ann}'")

            return result

        return wrapper

    return decorator
