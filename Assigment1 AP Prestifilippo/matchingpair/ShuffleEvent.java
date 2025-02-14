/* 
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package matchingpair;

import java.util.EventObject;

//Classe che implementa un evento di tipo Shuffle, generato dal Board ogni volta che l'utente preme il pulsante Shuffle.
//Incapsula un'array di interi che rappresenta i nuovi valori per tutte le carte dopo lo shuffle. Utile per fornire dettagli sull'evento al listener, che li userà nella funzione di gestione.
//Le carte e altri componenti che vogliono reagire allo shuffle(per esempio il Counter, il Controller) possono implementare ShuffleListener per essere notificate.
public class ShuffleEvent extends EventObject {
    private int[] values;
    // Al costruttore viene passato source, che rappresenta l'oggetto che genera l'evento (Board) e l'array di interi con i nuovi valori da assegnare alle carte.
    public ShuffleEvent(Object source, int[] values) {
        super(source);
        this.values = values;
    }

    public int[] getValues() {
        return values;
    }
}

// Interfaccia ShuffleListener, utilizzata per implementare l'AbstractObserver nel pattern Observer.
//Gli oggetti che implementano questa interfaccia vogliono essere notificati quando si verifica uno ShuffleEvent. Implementersnno il metodo onShuffle(ShuffleEvent e), 
//che verrà chiamato dal source al verificarsi dell'evento e servirà per la gestione di esso.
interface ShuffleListener {
    void onShuffle(ShuffleEvent e);
}
