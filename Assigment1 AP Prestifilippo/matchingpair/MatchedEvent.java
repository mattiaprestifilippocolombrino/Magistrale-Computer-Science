/* 
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package matchingpair;

import java.util.EventObject;

//Classe che implementa un evento di tipo Matched, generato dal Controller quando verifica che due carte Face_Up abbiano lo stesso valore e ascoltato dalle Card che si trovano nello stato FACE_UP
// Incapsula un campo booleano matched.
public class MatchedEvent extends EventObject {   
    private boolean matched;

    // Al costruttore viene passato source, che rappresenta l'oggetto che genera l'evento (Controller) e matched.
    public MatchedEvent(Object source, boolean matched) {
        super(source);
        this.matched = matched;
    }

    public boolean isMatched() {
        return matched;
    }
}

// Interfaccia MatchedListener, utilizzata per implementare l'AbstractObserver nel pattern Observer.
//Gli oggetti che implementano questa interfaccia vogliono essere notificati quando si verifica uno MatchEvent. Implementeranno il metodo onMatched(MatchedEvent e)), 
//che verrà chiamato dal source al verificarsi dell'evento e servirà per la gestione di esso.
interface MatchedListener {
    void onMatched(MatchedEvent e);
}
