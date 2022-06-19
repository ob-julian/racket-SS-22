import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;

/**
 * An interface for Person Managers.
 * 
 * Classes that implement the interface shall use something like this to store
 * persons: private List<Person> persons = new ArrayList<>();
 * 
 * Note: for simplicity we do not use generics for the interface type. you may
 * change the implementation to use generics.
 * 
 * @author andreas
 *
 */
public interface ManagerInterface {

	/** adds the given Person */
	boolean add(Person p);

	/** removes the given Person */
	boolean remove(Person p);

	/**
	 * returns the first Person that matches the given predicate, or null (if
	 * none matches); (note:we don't care what "first" means)
	 */
	Person getFirst(Predicate<Person> pred);

	/**
	 * returns an Optional that contains either the first person that matches
	 * the given predicate or contains null (if none matches);
	 * (note:we don't care what "first" means)
	 */
	Optional<Person> getFirstOptional(Predicate<Person> pred);

	/** returns a list of all persons that match the given predicate */
	List<Person> getAll(Predicate<Person> pred);

}