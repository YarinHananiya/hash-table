/*******************************************************************************
 * hash_table (fixed size buckets)
 * Author: Yarin Hananiya
 * ****************************************************************************/
#ifndef HASH_HPP
#define HASH_HPP

#include <algorithm>  // std::find_if, std::all_of
#include <array>      // std::array
#include <cstdint>    // __SIZE_MAX__
#include <functional> // std::equal_to, std::mem_fn
#include <iterator>   // std::forward_iterator_tag
#include <list>       // std::list
#include <numeric>    // std::accumulate
#include <tuple>      // std::tuple
#include <type_traits> // std::is_invocable_r_v, std::enable_if_t, std::conditional
#include <utility>     // std::pair
	
namespace container {

template<typename Key,
         typename Value,
         std::size_t N,
         typename HashFunc = std::hash<Key>, 
         typename BucketContainer = std::list<std::pair<const Key, Value>>,
         typename KeyEqual = std::equal_to<Key>,
         typename = std::enable_if_t<std::is_default_constructible_v<Value>>,
         typename = std::enable_if_t<(N < __SIZE_MAX__ - 1)>,
         typename = std::enable_if_t<std::is_invocable_r_v<std::size_t, HashFunc, Key>>,
         typename = std::enable_if_t<std::is_same_v<typename BucketContainer::iterator, 
                                                    decltype(std::begin(std::declval<std::add_lvalue_reference_t<BucketContainer>>()))> &&
                                     std::is_same_v<typename BucketContainer::iterator, 
                                                    decltype(std::end(std::declval<std::add_lvalue_reference_t<BucketContainer>>()))> &&
                                     std::is_same_v<void, 
                                                    decltype(std::declval<BucketContainer>().push_front(std::declval<std::pair<const Key, Value>>()))> &&
                                     std::is_same_v<typename BucketContainer::iterator, 
                                                    decltype(std::declval<BucketContainer>().erase(std::declval<typename BucketContainer::const_iterator>()))> &&
                                     std::is_same_v<bool, 
                                                    decltype(std::declval<BucketContainer>().empty())> &&
                                     std::is_same_v<std::size_t, 
                                                    decltype(std::declval<BucketContainer>().size())> &&
                                     std::is_same_v<typename BucketContainer::iterator::iterator_category, std::bidirectional_iterator_tag>>,
         typename = std::enable_if_t<std::is_invocable_r_v<bool, KeyEqual, Key, Key>> >
class hash_table {
public:
    hash_table() = default;
    hash_table(const hash_table&) = default;  
    hash_table(hash_table&&) noexcept = default; 
    hash_table& operator=(const hash_table&) = default;
    hash_table& operator=(hash_table&&) noexcept = default;
    ~hash_table() = default;

    using value_type = std::pair<const Key, Value>;
    template<bool IsConst> class Iterator; // forward declaration
    using iterator = Iterator<false>;
    using const_iterator = Iterator<true>;

    auto insert(const value_type& value) -> std::pair<bool, iterator>;
    auto erase(const Key& key) -> void;
    auto erase(const_iterator pos) -> iterator;
    auto find(const Key& key) -> iterator;
    auto find(const Key& key) const -> const_iterator;
    auto operator[](const Key& key) -> Value&;
    auto empty() const noexcept -> bool;
    auto size() const noexcept -> std::size_t;

    auto begin() noexcept -> iterator;
    auto begin() const noexcept -> const_iterator;
    auto cbegin() const noexcept -> const_iterator;
    auto end() noexcept -> iterator;
    auto end() const noexcept -> const_iterator;
    auto cend() const noexcept -> const_iterator;

public:
    template<bool IsConst>
    class Iterator {
    public: 
        using difference_type = void;
        using value_type = hash_table::value_type;
        using pointer = std::conditional<IsConst, const value_type*, value_type*>;
        using reference = std::conditional<IsConst, const value_type&, value_type&>;
        using iterator_category = std::forward_iterator_tag;

        using table_iterator = typename hash_table::table_iterator;
        using bucket_iterator = typename hash_table::bucket_iterator;

        Iterator() = default;
        Iterator(const Iterator&) = default;
        Iterator(Iterator&&) = default;
        template<bool IsConst_ = IsConst, typename = std::enable_if_t<IsConst_>>
        Iterator(const Iterator<false>& other);
        Iterator& operator=(const Iterator&) = default;
        Iterator& operator=(Iterator&&) noexcept = default;
        template<bool IsConst_ = IsConst, typename = std::enable_if_t<IsConst_>>
        Iterator& operator=(const Iterator<false>& other);
        ~Iterator() = default;

        auto operator++(int) -> const Iterator;
        auto operator++() -> Iterator&;
        auto operator*() -> value_type&;
        auto operator->() -> value_type*;

        friend bool operator==(const Iterator& lo, const Iterator& ro) {
            return (lo.m_table_iter == ro.m_table_iter &&
                    lo.m_bucket_iter == ro.m_bucket_iter);
        }

        friend bool operator!=(const Iterator& lo, const Iterator& ro) {
            return !(lo == ro);
        }
    
    private:
        explicit Iterator(hash_table& hasht,
                          table_iterator table_iter,
                          bucket_iterator bucket_iter);

        hash_table *m_hasht;
        table_iterator m_table_iter;
        bucket_iterator m_bucket_iter;

        friend class hash_table;
    };

private:
    constexpr static std::size_t num_of_buckets = N + 1;

    using table = std::array<BucketContainer, num_of_buckets>;
    using table_iterator = typename table::iterator;
    using bucket_iterator = typename BucketContainer::iterator;
    using found_value = std::tuple<bool, table_iterator, bucket_iterator>;

    auto get_table_iter(const Key& key) -> table_iterator;
    auto find_value(const Key& key) -> found_value;
    auto optimization_after_find(BucketContainer& table_pos, 
                                 bucket_iterator bucket_pos) noexcept -> bucket_iterator;
    auto get_dummy_table_iterator() -> table_iterator;
    auto get_next_non_empty_bucket(table_iterator pos) -> table_iterator;

    table m_table;
};

template<typename Key, typename Value, std::size_t N, typename HashFunc, typename BucketContainer,
         typename KeyEqual, typename A, typename B, typename C, typename D, typename E>
auto hash_table<Key, Value, N, HashFunc, BucketContainer, KeyEqual, A, B, C, D, E>::
    insert(const value_type& value) -> std::pair<bool, hash_table::iterator> {
        bool is_found;
        table_iterator table_iter;
        bucket_iterator bucket_iter;
        
        std::tie(is_found, table_iter, bucket_iter) = find_value(value.first);
        if (!is_found) {
            table_iter->push_front(value);
            bucket_iter = std::begin(*table_iter);
        }

        return std::make_pair(is_found, iterator(*this, table_iter, bucket_iter));
}
 
template<typename Key, typename Value, std::size_t N, typename HashFunc, typename BucketContainer,
         typename KeyEqual, typename A, typename B, typename C, typename D, typename E>
auto hash_table<Key, Value, N, HashFunc, BucketContainer, KeyEqual, A, B, C, D, E>::
    erase(const Key& key) -> void {
        bool is_found;
        table_iterator table_iter;
        bucket_iterator bucket_iter;
        
        std::tie(is_found, table_iter, bucket_iter) = find_value(key);
        if (is_found) {
            table_iter->erase(bucket_iter);
        }
}

template<typename Key, typename Value, std::size_t N, typename HashFunc, typename BucketContainer,
         typename KeyEqual, typename A, typename B, typename C, typename D, typename E>
auto hash_table<Key, Value, N, HashFunc, BucketContainer, KeyEqual, A, B, C, D, E>::
    erase(const_iterator pos) -> iterator {
        auto is_equal_iterator = [&pos](const iterator& iter){ return iter == pos; };
        auto found_iter = std::find_if(begin(), end(), is_equal_iterator);
        if (end() != found_iter) {
            auto next_iter = ++found_iter;
            pos.m_table_iter->erase(pos.m_bucket_iter);
            return next_iter;
        }

        return end();
}

template<typename Key, typename Value, std::size_t N, typename HashFunc, typename BucketContainer,
         typename KeyEqual, typename A, typename B, typename C, typename D, typename E>
auto hash_table<Key, Value, N, HashFunc, BucketContainer, KeyEqual, A, B, C, D, E>::
    find(const Key& key) -> iterator {
        bool is_found;
        table_iterator table_iter;
        bucket_iterator bucket_iter;

        std::tie(is_found, table_iter, bucket_iter) = find_value(key);
        if (!is_found) {
            return end();
        }
            
        return iterator(*this, table_iter, optimization_after_find(*table_iter, bucket_iter));
}

template<typename Key, typename Value, std::size_t N, typename HashFunc, typename BucketContainer,
         typename KeyEqual, typename A, typename B, typename C, typename D, typename E>
auto hash_table<Key, Value, N, HashFunc, BucketContainer, KeyEqual, A, B, C, D, E>::
    find(const Key& key) const -> const_iterator {
        return (const_cast<hash_table*>(this))->find(key);       
}

template<typename Key, typename Value, std::size_t N, typename HashFunc, typename BucketContainer,
         typename KeyEqual, typename A, typename B, typename C, typename D, typename E>
auto hash_table<Key, Value, N, HashFunc, BucketContainer, KeyEqual, A, B, C, D, E>::
    operator[](const Key& key) -> Value& {
        bool is_found;
        table_iterator table_iter;
        bucket_iterator bucket_iter;

        std::tie(is_found, table_iter, bucket_iter) = find_value(key);
        if (!is_found) {
            table_iter->push_front(value_type(key, Value()));
            return std::begin(*table_iter)->second;
        }

        return optimization_after_find(*table_iter, bucket_iter)->second;
}

template<typename Key, typename Value, std::size_t N, typename HashFunc, typename BucketContainer,
         typename KeyEqual, typename A, typename B, typename C, typename D, typename E>
auto hash_table<Key, Value, N, HashFunc, BucketContainer, KeyEqual, A, B, C, D, E>::
    empty() const noexcept -> bool {
        return std::all_of(m_table.begin(), m_table.end(), std::mem_fn(&BucketContainer::empty));
}

template<typename Key, typename Value, std::size_t N, typename HashFunc, typename BucketContainer,
         typename KeyEqual, typename A, typename B, typename C, typename D, typename E>
auto hash_table<Key, Value, N, HashFunc, BucketContainer, KeyEqual, A, B, C, D, E>::
    size() const noexcept -> std::size_t {
        auto sum_buckets = 
            [](std::size_t curr_sum, const BucketContainer& bucket) { 
                return curr_sum + bucket.size(); 
        };
        return std::accumulate(m_table.begin(), m_table.end(), 0ul, sum_buckets);
}   

template<typename Key, typename Value, std::size_t N, typename HashFunc, typename BucketContainer,
         typename KeyEqual, typename A, typename B, typename C, typename D, typename E>
auto hash_table<Key, Value, N, HashFunc, BucketContainer, KeyEqual, A, B, C, D, E>::
    begin() noexcept -> iterator {
        auto first_non_empty_bucket = get_next_non_empty_bucket(m_table.begin());
        return iterator(*this, first_non_empty_bucket, std::begin(*first_non_empty_bucket));
}

template<typename Key, typename Value, std::size_t N, typename HashFunc, typename BucketContainer,
         typename KeyEqual, typename A, typename B, typename C, typename D, typename E>
auto hash_table<Key, Value, N, HashFunc, BucketContainer, KeyEqual, A, B, C, D, E>::
    begin() const noexcept -> const_iterator {
        return cbegin();
}

template<typename Key, typename Value, std::size_t N, typename HashFunc, typename BucketContainer,
         typename KeyEqual, typename A, typename B, typename C, typename D, typename E>
auto hash_table<Key, Value, N, HashFunc, BucketContainer, KeyEqual, A, B, C, D, E>::
    cbegin() const noexcept -> const_iterator {
        return (const_cast<hash_table*>(this))->begin();
}

template<typename Key, typename Value, std::size_t N, typename HashFunc, typename BucketContainer,
         typename KeyEqual, typename A, typename B, typename C, typename D, typename E>
auto hash_table<Key, Value, N, HashFunc, BucketContainer, KeyEqual, A, B, C, D, E>::
    end() noexcept -> iterator {
        auto dummy_table_iter = get_dummy_table_iterator();
        return iterator(*this, dummy_table_iter, std::begin(*dummy_table_iter));
}

template<typename Key, typename Value, std::size_t N, typename HashFunc, typename BucketContainer,
         typename KeyEqual, typename A, typename B, typename C, typename D, typename E>
auto hash_table<Key, Value, N, HashFunc, BucketContainer, KeyEqual, A, B, C, D, E>::
    end() const noexcept -> const_iterator {
        return cend();
}

template<typename Key, typename Value, std::size_t N, typename HashFunc, typename BucketContainer,
         typename KeyEqual, typename A, typename B, typename C, typename D, typename E>
auto hash_table<Key, Value, N, HashFunc, BucketContainer, KeyEqual, A, B, C, D, E>::
    cend() const noexcept -> const_iterator {
        return (const_cast<hash_table*>(this))->end();
}

template<typename Key, typename Value, std::size_t N, typename HashFunc, typename BucketContainer,
         typename KeyEqual, typename A, typename B, typename C, typename D, typename E>
auto hash_table<Key, Value, N, HashFunc, BucketContainer, KeyEqual, A, B, C, D, E>::
    get_table_iter(const Key& key) -> table_iterator {

        return m_table.begin() + (HashFunc()(key) % N);
}

template<typename Key, typename Value, std::size_t N, typename HashFunc, typename BucketContainer,
         typename KeyEqual, typename A, typename B, typename C, typename D, typename E>
auto hash_table<Key, Value, N, HashFunc, BucketContainer, KeyEqual, A, B, C, D, E>::
    find_value(const Key& key) -> found_value {
        auto table_iter = get_table_iter(key);
        auto is_equal_to_key = [&key](const value_type& v){ return KeyEqual()(key, v.first); };
        auto found_bucket_iter = std::find_if(std::begin(*table_iter),
                                              std::end(*table_iter), 
                                              is_equal_to_key);
        return found_value(found_bucket_iter != std::end(*table_iter), table_iter, found_bucket_iter);
}

template<typename Key, typename Value, std::size_t N, typename HashFunc, typename BucketContainer,
         typename KeyEqual, typename A, typename B, typename C, typename D, typename E>
auto hash_table<Key, Value, N, HashFunc, BucketContainer, KeyEqual, A, B, C, D, E>::
    optimization_after_find(BucketContainer& table_pos, 
                            bucket_iterator found_bucket_iter) noexcept -> bucket_iterator {
        try {
            table_pos.push_front(*found_bucket_iter);
            table_pos.erase(found_bucket_iter);
            found_bucket_iter = std::begin(table_pos);
        }
        catch(const std::exception& e) {} 

        return found_bucket_iter;
}

template<typename Key, typename Value, std::size_t N, typename HashFunc, typename BucketContainer,
         typename KeyEqual, typename A, typename B, typename C, typename D, typename E>
auto hash_table<Key, Value, N, HashFunc, BucketContainer, KeyEqual, A, B, C, D, E>::
    get_dummy_table_iterator() -> table_iterator {
        return std::prev(m_table.end());
}

template<typename Key, typename Value, std::size_t N, typename HashFunc, typename BucketContainer,
         typename KeyEqual, typename A, typename B, typename C, typename D, typename E>
auto hash_table<Key, Value, N, HashFunc, BucketContainer, KeyEqual, A, B, C, D, E>::
    get_next_non_empty_bucket(table_iterator pos) -> table_iterator {
        return std::find_if_not(pos,
                                get_dummy_table_iterator(), 
                                std::mem_fn(&BucketContainer::empty));
}

template<typename Key, typename Value, std::size_t N, typename HashFunc, typename BucketContainer,
         typename KeyEqual, typename A, typename B, typename C, typename D, typename E>
template<bool IsConst>
hash_table<Key, Value, N, HashFunc, BucketContainer, KeyEqual, A, B, C, D, E>::Iterator<IsConst>::
    Iterator(hash_table& hasht, table_iterator table_iter, bucket_iterator bucket_iter) 
: m_hasht(&hasht)
, m_table_iter(table_iter)
, m_bucket_iter(bucket_iter) {}

template<typename Key, typename Value, std::size_t N, typename HashFunc, typename BucketContainer,
         typename KeyEqual, typename A, typename B, typename C, typename D, typename E>
template<bool IsConst>
template<bool IsConst_, typename>
hash_table<Key, Value, N, HashFunc, BucketContainer, KeyEqual, A, B, C, D, E>::Iterator<IsConst>::
    Iterator(const Iterator<false>& other) 
: m_hasht(other.m_hasht)
, m_table_iter(other.m_table_iter)
, m_bucket_iter(other.m_bucket_iter) {}


template<typename Key, typename Value, std::size_t N, typename HashFunc, typename BucketContainer,
         typename KeyEqual, typename A, typename B, typename C, typename D, typename E>
template<bool IsConst>
template<bool IsConst_, typename>
auto hash_table<Key, Value, N, HashFunc, BucketContainer, KeyEqual, A, B, C, D, E>::Iterator<IsConst>::
    operator=(const Iterator<false>& other) -> Iterator& {
        m_hasht = other.m_hasht;
        m_table_iter = other.m_table_iter;
        m_bucket_iter = other.m_bucket_iter;

    return *this;
}

template<typename Key, typename Value, std::size_t N, typename HashFunc, typename BucketContainer,
         typename KeyEqual, typename A, typename B, typename C, typename D, typename E>
template<bool IsConst>
auto hash_table<Key, Value, N, HashFunc, BucketContainer, KeyEqual, A, B, C, D, E>::Iterator<IsConst>::
    operator++(int) -> const Iterator {
        auto old_val = *this;
        ++(*this);
        return old_val;
}

template<typename Key, typename Value, std::size_t N, typename HashFunc, typename BucketContainer,
         typename KeyEqual, typename A, typename B, typename C, typename D, typename E>
template<bool IsConst>
auto hash_table<Key, Value, N, HashFunc, BucketContainer, KeyEqual, A, B, C, D, E>::Iterator<IsConst>::
    operator++() -> Iterator& {
        ++m_bucket_iter;
        if (std::end(*m_table_iter) == m_bucket_iter) {
            m_table_iter = m_hasht->get_next_non_empty_bucket(++m_table_iter);
            m_bucket_iter = std::begin(*m_table_iter);
        }

        return *this;
}

template<typename Key, typename Value, std::size_t N, typename HashFunc, typename BucketContainer,
         typename KeyEqual, typename A, typename B, typename C, typename D, typename E>
template<bool IsConst>
auto hash_table<Key, Value, N, HashFunc, BucketContainer, KeyEqual, A, B, C, D, E>::Iterator<IsConst>::
    operator*() -> value_type& {
        return *m_bucket_iter;
}

template<typename Key, typename Value, std::size_t N, typename HashFunc, typename BucketContainer,
         typename KeyEqual, typename A, typename B, typename C, typename D, typename E>
template<bool IsConst>
auto hash_table<Key, Value, N, HashFunc, BucketContainer, KeyEqual, A, B, C, D, E>::Iterator<IsConst>::
    operator->() -> value_type* {
        return &*m_bucket_iter;
}

} // namespace container

#endif