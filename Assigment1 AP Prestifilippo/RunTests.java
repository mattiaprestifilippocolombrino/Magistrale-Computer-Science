/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package runtest;


import java.lang.annotation.Annotation;
import java.lang.reflect.*;
import java.util.Arrays;


public class RunTests {
    public static void main(String[] args) {
        // Controlla che ci sia esattamente un argomento: il nome della classe da testare
        if (args.length != 1) {
            System.err.println("Usage: java RunTests <className>");
            System.exit(1);
        }

        String className = args[0];

        /*
        Carica la classe dinamicamente a runtime usando reflection. Crea una nuova istanza della classe, invocando il costruttore di default
        Itera su tutti i metodi dichiarati nella classe. Per ogni metodo: Ottiene i modificatori del metodo, e
        considera i metodi non privati e che hanno l'annotazione @Testable. Recupera l'annotazione @Specification associata al metodo.
        Esegue il test sul metodo con le specifiche indicate.
        */
        try {
            
            Class<?> cls = Class.forName(className);
            Object instance = cls.getConstructor().newInstance();
            for (Method m : cls.getDeclaredMethods()) {                
                int modifiers = m.getModifiers();               
                if (!Modifier.isPrivate(modifiers) && m.isAnnotationPresent(Testable.class)) { 
                    Specification spec = m.getAnnotation(Specification.class);
                    performTest(instance, m, spec);
                }
            }
        } catch (ClassNotFoundException e) {
            System.err.println("Class not found: " + className);
        } catch (NoSuchMethodException e) {
            System.err.println("No default constructor found for class: " + className);
        } catch (InstantiationException | IllegalAccessException | InvocationTargetException e) {
            System.err.println("Error instantiating class: " + e.getMessage());
        }
    }

    
    /*
    Recupera i tipi dei parametri dal metodo m, e tipi e valori dall'annotation spec. Se il numero di argomenti dichiarati nell'annotazione
    non corrisponde a quelli del metodo, si segnala un errore (WrongArgs).
    Per ogni parametro, converte gli argomenti da stringhe a oggetti del tipo richiesto dal metodo, usando la funzione convertValue.
    Se la conversione fallisce si segnala un errore.
    */
    private static void performTest(Object instance, Method m, Specification spec) {
        Class<?>[] paramTypes = m.getParameterTypes();
        String[] argTypes = spec.argTypes();
        String[] argValues = spec.argValues();
        if (argTypes.length != paramTypes.length || argValues.length != paramTypes.length) {
            Report.report(Report.TEST_RESULT.WrongArgs, m.getName(), spec);
            return;
        }
        Object[] convertedArgs = new Object[paramTypes.length];
        for (int i = 0; i < paramTypes.length; i++) {
            Object converted;
            try {
                converted = convertValue(argTypes[i], argValues[i], paramTypes[i]);
            } catch (Exception e) {
                // Se conversione fallisce, WrongArgs
                Report.report(Report.TEST_RESULT.WrongArgs, m.getName(), spec);
                return;
            }
            if (converted == null) {
                // Tipo non corrisponde, WrongArgs
                Report.report(Report.TEST_RESULT.WrongArgs, m.getName(), spec);
                return;
            }
            convertedArgs[i] = converted;
        }

        // Viene invocato il metodo
        Object result = null;
        try {
            result = m.invoke(instance, convertedArgs);
        } catch (IllegalAccessException | InvocationTargetException e) {
            Report.report(Report.TEST_RESULT.WrongArgs, m.getName(), spec);
            return;
        }

        // Viene controllato il tipo di ritorno atteso. Se il metodo è void, nel caso viene ritornato qualcosa, si segnala errore. 
        //Se il metodo ritorna qualcosa, se non ritorna risultati o se il tipo ritornato è diverso da quello aspettato, si segnala errore.
        Class<?> returnType = m.getReturnType();
        String expectedResType = spec.resType();
        String expectedResVal = spec.resVal();

        // Se il metodo è void
        if (returnType.equals(void.class)) {
            if (!expectedResType.isEmpty()) {
                // Ci si aspettava un risultato ma il metodo è void
                Report.report(Report.TEST_RESULT.WrongResultType, m.getName(), spec);
                return;
            } else {
                // Metodo void e nessun risultato atteso -> Test passato
                Report.report(Report.TEST_RESULT.TestSucceeded, m.getName(), spec);
                return;
            }
        } else {
            // Il metodo ritorna qualcosa
            if (expectedResType.isEmpty()) {
                // Non ci si aspettava un risultato ma il metodo ne ha uno
                Report.report(Report.TEST_RESULT.WrongResultType, m.getName(), spec);
                return;
            }

            // Converte il risultato atteso nel tipo effettivo del metodo
            Object expectedValue;
            try {
                expectedValue = convertExpectedResult(expectedResType, expectedResVal, returnType);
            } catch (Exception e) {
                // Conversione non riuscita -> WrongResultType
                Report.report(Report.TEST_RESULT.WrongResultType, m.getName(), spec);
                return;
            }

            if (expectedValue == null) {
                // Tipo atteso non corrisponde
                Report.report(Report.TEST_RESULT.WrongResultType, m.getName(), spec);
                return;
            }

            // Se il risultato atteso è diverso da quello ottenuto, il test è fallito, altrimenti ha successo
            if (!compareValues(result, expectedValue)) {
                // Risultato diverso da atteso
                Report.report(Report.TEST_RESULT.TestFailed, m.getName(), spec);
            } else {
                // Risultato corretto
                Report.report(Report.TEST_RESULT.TestSucceeded, m.getName(), spec);
            }
        }
    }

    // Converte gli argomenti da stringa al tipo richiesto dal metodo testato.
    private static Object convertValue(String argType, String argValue, Class<?> paramType) {
        switch (argType) {
            case "int":
                if (paramType.equals(int.class) || paramType.equals(Integer.class)) {
                    try {
                        return Integer.valueOf(argValue);
                    } catch (NumberFormatException e) {
                        return null;
                    }
                } else {
                    return null;
                }
            case "double":
                if (paramType.equals(double.class) || paramType.equals(Double.class)) {
                    try {
                        return Double.valueOf(argValue);
                    } catch (NumberFormatException e) {
                        return null;
                    }
                } else {
                    return null;
                }
            case "bool":
                if (paramType.equals(boolean.class) || paramType.equals(Boolean.class)) {
                    if (argValue.equals("true") || argValue.equals("false")) {
                        return Boolean.valueOf(argValue);
                    } else {
                        return null;
                    }
                } else {
                    return null;
                }
            case "string":
                if (paramType.equals(String.class)) {
                    return argValue;
                } else {
                    return null;
                }
            default:
                return null;
        }
    }

    // Converte il risultato atteso in un oggetto del tipo di ritorno del metodo
    private static Object convertExpectedResult(String resType, String resVal, Class<?> actualRetType) {
        if (resType.equals("int")) {
            if (actualRetType.equals(int.class) || actualRetType.equals(Integer.class)) {
                try {
                    return Integer.valueOf(resVal);
                } catch (NumberFormatException e) {
                    return null;
                }
            } else {
                return null;
            }
        } else if (resType.equals("double")) {
            if (actualRetType.equals(double.class) || actualRetType.equals(Double.class)) {
                try {
                    return Double.valueOf(resVal);
                } catch (NumberFormatException e) {
                    return null;
                }
            } else {
                return null;
            }
        } else if (resType.equals("bool")) {
            if (actualRetType.equals(boolean.class) || actualRetType.equals(Boolean.class)) {
                if (resVal.equals("true") || resVal.equals("false")) {
                    return Boolean.valueOf(resVal);
                } else {
                    return null;
                }
            } else {
                return null;
            }
        } else if (resType.equals("string")) {
            if (actualRetType.equals(String.class)) {
                return resVal;
            } else {
                return null;
            }
        } else if (resType.isEmpty()) {
            // Nessun risultato atteso, ma il metodo non è void -> errore gestito altrove
            return null;
        } else {
            // Tipo sconosciuto
            return null;
        }
    }

    // Confronta i valori ottenuti con quelli attesi usando equals
    private static boolean compareValues(Object actual, Object expected) {
        if (actual == null && expected == null) {
            return true;
        }
        if (actual == null || expected == null) {
            return false;
        }
        return actual.equals(expected);
    }
}
