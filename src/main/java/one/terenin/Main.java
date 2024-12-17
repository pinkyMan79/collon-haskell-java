package one.terenin;

import java.util.*;
import java.util.function.Consumer;

// Класс для представления ошибки
class ColonError extends Exception {
    public ColonError(String message) {
        super(message);
    }
}

// Класс для работы со стеком
class Stack {
    private LinkedList<Integer> stack = new LinkedList<>();

    public void push(int value) {
        stack.push(value);
    }

    public int pop() throws ColonError {
        if (stack.isEmpty()) {
            throw new ColonError("Stack underflow");
        }
        return stack.pop();
    }

    public int peek() throws ColonError {
        if (stack.isEmpty()) {
            throw new ColonError("Stack underflow");
        }
        return stack.peek();
    }

    public void printStack() {
        System.out.println(stack);
    }
}

// Класс для представления памяти
class Memory {
    private Map<String, Integer> memory = new HashMap<>();

    public void addVariable(String name, int value) {
        memory.put(name, value);
    }

    public Integer getVariable(String name) {
        return memory.get(name);
    }

    public boolean contains(String name) {
        return memory.containsKey(name);
    }
}

// Интерфейс для команд
interface Command {
    void execute(Stack stack, Memory memory) throws ColonError;
}

// Реализация арифметических операций
class AddCommand implements Command {
    @Override
    public void execute(Stack stack, Memory memory) throws ColonError {
        int b = stack.pop();
        int a = stack.pop();
        stack.push(a + b);
    }
}

class SubtractCommand implements Command {
    @Override
    public void execute(Stack stack, Memory memory) throws ColonError {
        int b = stack.pop();
        int a = stack.pop();
        stack.push(a - b);
    }
}

class MultiplyCommand implements Command {
    @Override
    public void execute(Stack stack, Memory memory) throws ColonError {
        int b = stack.pop();
        int a = stack.pop();
        stack.push(a * b);
    }
}

class DivideCommand implements Command {
    @Override
    public void execute(Stack stack, Memory memory) throws ColonError {
        int b = stack.pop();
        int a = stack.pop();
        if (b == 0) throw new ColonError("Division by zero");
        stack.push(a / b);
    }
}

// Команды для работы с переменными
class SetVariableCommand implements Command {
    private String name;

    public SetVariableCommand(String name) {
        this.name = name;
    }

    @Override
    public void execute(Stack stack, Memory memory) throws ColonError {
        int value = stack.pop();
        memory.addVariable(name, value);
    }
}

class GetVariableCommand implements Command {
    private String name;

    public GetVariableCommand(String name) {
        this.name = name;
    }

    @Override
    public void execute(Stack stack, Memory memory) throws ColonError {
        Integer value = memory.getVariable(name);
        if (value == null) {
            throw new ColonError("Variable " + name + " not defined");
        }
        stack.push(value);
    }
}

// Условный оператор
class IfCommand implements Command {
    private List<Command> ifCommands;
    private List<Command> elseCommands;

    public IfCommand(List<Command> ifCommands, List<Command> elseCommands) {
        this.ifCommands = ifCommands;
        this.elseCommands = elseCommands;
    }

    @Override
    public void execute(Stack stack, Memory memory) throws ColonError {
        int condition = stack.pop();
        List<Command> commandsToExecute = condition != 0 ? ifCommands : elseCommands;
        for (Command command : commandsToExecute) {
            command.execute(stack, memory);
        }
    }
}

// Цикл
class DoLoopCommand implements Command {
    private int start;
    private int end;
    private List<Command> loopCommands;

    public DoLoopCommand(int start, int end, List<Command> loopCommands) {
        this.start = start;
        this.end = end;
        this.loopCommands = loopCommands;
    }

    @Override
    public void execute(Stack stack, Memory memory) throws ColonError {
        for (int i = start; i <= end; i++) {
            stack.push(i);
            for (Command command : loopCommands) {
                command.execute(stack, memory);
            }
        }
    }
}

// Команда для индекса в цикле
class ICommand implements Command {
    @Override
    public void execute(Stack stack, Memory memory) throws ColonError {
        int index = stack.peek();
        stack.push(index);
    }
}

// Класс для интерпретатора
class ColonInterpreter {
    private Stack stack = new Stack();
    private Memory memory = new Memory();

    public void execute(List<Command> commands) throws ColonError {
        for (Command command : commands) {
            command.execute(stack, memory);
        }
    }

    public void printStack() {
        stack.printStack();
    }
}

public class Main {
    public static void main(String[] args) {
        try {
            ColonInterpreter interpreter = new ColonInterpreter();

            // Пример: 1 2 + (сложение 1 и 2)
            List<Command> program = Arrays.asList(
                    new AddCommand()  // Складываем 1 и 2
            );
            interpreter.execute(program);
            interpreter.printStack(); // Должно вывести [3]

            // Пример с условием: если 1, то выводим "True", если 0 - "False"
            program = Arrays.asList(
                    new SetVariableCommand("condition"), // Устанавливаем переменную condition
                    new GetVariableCommand("condition"),
                    new IfCommand(
                            Arrays.asList(new SetVariableCommand("True")), // Если true, выполняем это
                            Arrays.asList(new SetVariableCommand("False")) // Если false, выполняем это
                    )
            );
            interpreter.execute(program);
            interpreter.printStack(); // Ожидаемый результат: [True]

            // Пример цикла: выводим числа от 1 до 5
            program = Arrays.asList(
                    new DoLoopCommand(1, 5, Arrays.asList(new SetVariableCommand("Number"))) // Цикл с числами от 1 до 5
            );
            interpreter.execute(program);
            interpreter.printStack(); // Ожидаемый результат: [1, 2, 3, 4, 5]

        } catch (ColonError e) {
            System.out.println("Error: " + e.getMessage());
        }
    }
}
