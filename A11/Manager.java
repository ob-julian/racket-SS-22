import javax.swing.text.html.Option;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;

public class Manager implements ManagerInterface{
    private List<Person> persons = new ArrayList<>();

    @Override
    public boolean add(Person p) {
        return persons.add(p);
    }

    @Override
    public boolean remove(Person p) {
        return persons.remove(p);
    }

    @Override
    public Person getFirst(Predicate<Person> pred) {
        return getFirstOptional(pred).orElse(null);
    }

    @Override
    public Optional<Person> getFirstOptional(Predicate<Person> pred) {
        return persons.stream().filter(pred).findFirst();
    }

    @Override
    public List<Person> getAll(Predicate<Person> pred) {
        return persons.stream().filter(pred).toList();
    }
}
