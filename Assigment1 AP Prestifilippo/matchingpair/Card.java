package matchingpair;

import javax.swing.JButton;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeSupport;
import java.beans.VetoableChangeSupport;
import java.beans.VetoableChangeListener;
import java.beans.PropertyChangeListener;
import java.beans.PropertyVetoException;



/*
Classe Card sottoclasse di JButton.
Ogni carta ha due proprietà: value e state, che può essere FACE_DOWN, FACE_UP e EXCLUDED.
State è sia Bound che Constrained: cambi di stato notificano i PropertyChangeListener(Controller e counter) e possono essere vetoati dai VetoableChangeListener.
Un click utente su una carta FACE_DOWN la girerà FACE_UP, uno su una carta FACE_UP o EXCLUDED tenterà di riportarla FACE_DOWN. 
Il Controller può vetoare quest'ultima azione se non è consentita.
Implementa le interfacce ShuffleListener e MatchedListener, che fungono da AbstractOsserver nel modello Observer, rispettivamente per gli eventi di Shuffle e di Match.
*/
public class Card extends JButton implements ShuffleListener, MatchedListener {
    // PropertyChangeSupport per gestire gli eventi di proprietà bound, come value e state
    private PropertyChangeSupport pcs = new PropertyChangeSupport(this);
    // VetoableChangeSupport per gestire gli eventi di proprietà constrained, come state
    private VetoableChangeSupport vcs = new VetoableChangeSupport(this);
    private int value;
    private CardState state = CardState.FACE_DOWN; 

    private boolean internalChange = false; // variabile che indica se il cambiamento di stato è interno (causato da un evento), o da un utente

    //Chiama il costruttore di JButton, inizializza l'aspetto grafico della carta e imposta l'ActionListener, 
    //ovvero il metodo che deve essere eseguito quando il pulsante viene cliccato
    public Card() {
        super();
        updateVisualState();

        //Quando viene cliccato dall'utente, si imposta internalChange = false. Se la carta è FACE_DOWN, si prova a girare FACE_UP.
        //Se era FACE_UP o EXCLUDED, si prova a riportarla FACE_DOWN. Se il Controller (VetoableChangeListener) ha posto veto, il cambiamento non avviene 
        addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                try {
                    internalChange = false;
                    if (state == CardState.FACE_DOWN) {
                        setState(CardState.FACE_UP);
                    } else if (state == CardState.FACE_UP || state == CardState.EXCLUDED) {
                        setState(CardState.FACE_DOWN);
                    }
                } catch (PropertyVetoException pve) {
                    // Non viene fatto nulla: il veto impedisce il cambio di stato
                }
            }
        });
    }

    public int getValue() { 
        return value; 
    }

    
    // Setter della bound property value: Notifica i listener di PropertyChange del cambiamento di "value" e aggiorna l'aspetto della carta dopo il cambiamento
    public void setValue(int newValue) {
        int oldValue = this.value;
        this.value = newValue;
        pcs.firePropertyChange("value", oldValue, this.value);
        updateVisualState();
    }

    public CardState getStateCard() { 
        return state; 
    }

    // Setter della proprietà bound e constrained state. Prima di cambiare stato, chiede al controller se approva il cambio. 
    // Se non viene posto veto, lo stato viene aggiornato e vengono notificati i PropertyChangeListener (Controller e Counter) del cambiamento di stato
    public void setState(CardState newState) throws PropertyVetoException {
        CardState oldState = this.state;
        vcs.fireVetoableChange("state", oldState, newState);
        this.state = newState;
        pcs.firePropertyChange("state", oldState, newState);
        updateVisualState();
    }

    public boolean isInternalChange() {
        return internalChange;
    }

    
    // Aggiorna l'aspetto grafico della carta in base allo stato. Se lo stato è face_down, viene impostato testo vuoto e sfondo verde.  
    // Se face_up, mostra il valore e sfondo bianco.Se excluded, mostra il valore e sfondo rosso
    private void updateVisualState() {
        switch (this.state) {
            case FACE_DOWN:
                setText("");
                setBackground(Color.GREEN);
                break;
            case FACE_UP:
                setText(String.valueOf(value));
                setBackground(Color.WHITE);
                break;
            case EXCLUDED:
                setText(String.valueOf(value));
                setBackground(Color.RED);
                break;
        }
        repaint();
    }


    public void addBeanPropertyChangeListener(PropertyChangeListener l) {
        pcs.addPropertyChangeListener(l);
    }
    public void removeBeanPropertyChangeListener(PropertyChangeListener l) {
        pcs.removePropertyChangeListener(l);
    }

    public void addBeanVetoableChangeListener(VetoableChangeListener l) {
        vcs.addVetoableChangeListener(l);
    }
    public void removeBeanVetoableChangeListener(VetoableChangeListener l) {
        vcs.removeVetoableChangeListener(l);
    }

    // Metodo chiamato quando arriva un evento di tipo ShuffleEvent. La carta deve tornare face_down.
    @Override
    public void onShuffle(ShuffleEvent e) {
        internalChange = true;
        try {
            setState(CardState.FACE_DOWN);
        } catch (PropertyVetoException ex) {
            // Non dovrebbe accadere, il Controller non dovrebbe vetoare cambi interni
        } finally {
            internalChange = false;
        }
    }

     //Metodo chiamato quando arriva un evento di tipo MatchedEvent. Se la carta è c'è un match, la carta diventa EXCLUDED (rossa).
     // Se non è un match, torna FACE_DOWN (verde)
    @Override
    public void onMatched(MatchedEvent e) {
        if (this.state == CardState.FACE_UP) {
            boolean matched = e.isMatched();
            internalChange = true; 
            try {
                if (matched) {
                    setState(CardState.EXCLUDED);
                } else {
                    setState(CardState.FACE_DOWN);
                }
            } catch (PropertyVetoException pve) {
                // Non dovrebbe accadere, nessun veto sugli eventi interni
            } finally {
                internalChange = false;
            }
        }
    }
}
