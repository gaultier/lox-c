/* hashtab.h - Simple, Reliable C Hashtable
 */

/* Only copies of keys and values are stored in the table, not the
 * orignals.
 *
 * The hashtable does not grow automatically, but when the hashtable
 * grow function is called. Growing the hashtable is a safe operation:
 * if growing the hashtable fails, the existing hashtable is not
 * destroyed or modified.
 *
 * This hashtable is not thread-safe.
 */

#ifndef HASHTAB_H
#define HASHTAB_H

#include <stdlib.h>
#include <string.h>

typedef struct hashtab_node_t {
    void *key;     /* key for the node */
    size_t keylen; /* length of the key */
    void *value;   /* value for this node */
    size_t vallen; /* length of the value */

    struct hashtab_node_t *next; /* next node (open hashtable) */
} hashtab_node_t;

typedef struct hashtab_t {
    hashtab_node_t **arr;
    size_t size; /* size of the hash */
    int count;   /* number if items in this table */
    int (*hash_func)(void *, size_t, size_t); /* hash function */
} hashtab_t;

/* Iterator type for iterating through the hashtable. */
typedef struct hashtab_iter_t {
    /* key and value of current item */
    void *key;
    void *value;
    size_t keylen;
    size_t vallen;

    /* bookkeeping data */
    struct hashtab_internal_t {
        hashtab_t *hashtable;
        hashtab_node_t *node;
        int index;
    } internal;

} hashtab_iter_t;

/* Initialize a new hashtable (set bookingkeeping data) and return a
 * pointer to the hashtable. A hash function may be provided. If no
 * function pointer is given (a NULL pointer), then the built in hash
 * function is used. A NULL pointer returned if the creation of the
 * hashtable failed due to a failed malloc(). */
hashtab_t *ht_init(size_t size,
                   int (*hash_func)(void *key, size_t keylen, size_t ht_size));

/* Fetch a value from table matching the key. Returns a pointer to
 * the value matching the given key. */
void *ht_search(hashtab_t *hashtable, void *key, size_t keylen);

/* Put a value into the table with the given key. Returns NULL if
 * malloc() fails to allocate memory for the new node. */
void *ht_insert(hashtab_t *hashtable, void *key, size_t keylen, void *value,
                size_t vallen);

/* Delete the given key and value pair from the hashtable. If the key
 * does not exist, no error is given. */
void ht_remove(hashtab_t *hashtable, void *key, size_t keylen);

/* Change the size of the hashtable. It will allocate a new hashtable
 * and move all keys and values over. The pointer to the new hashtable
 * is returned. Will return NULL if the new hashtable fails to be
 * allocated. If this happens, the old hashtable will not be altered
 * in any way. The old hashtable is destroyed upon a successful
 * grow. */
void *ht_grow(hashtab_t *hashtable, size_t new_size);

/* Free all resources used by the hashtable. */
void ht_destroy(hashtab_t *hashtable);

/* Initialize the given iterator. It will point to the first element
 * in the hashtable. */
void ht_iter_init(hashtab_t *hashtable, hashtab_iter_t *ii);

/* Increment the iterator to the next element. The iterator key and
 * value will point to NULL values when the iterator has reached the
 * end of the hashtable.  */
void ht_iter_inc(hashtab_iter_t *ii);

/* Default hashtable hash function. */
int ht_hash(void *key, size_t key_size, size_t hashtab_size);

hashtab_t *ht_init(size_t size, int (*hash_func)(void *, size_t, size_t)) {
    hashtab_t *new_ht = (hashtab_t *)malloc(sizeof(hashtab_t));

    new_ht->arr = (hashtab_node_t **)malloc(sizeof(hashtab_node_t *) * size);

    new_ht->size = size;
    new_ht->count = 0;

    /* all entries are empty */
    int i = 0;
    for (i = 0; i < (int)size; i++) {
        new_ht->arr[i] = NULL;
    }

    if (hash_func == NULL)
        new_ht->hash_func = &ht_hash;
    else
        new_ht->hash_func = hash_func;

    return new_ht;
}

void *ht_search(hashtab_t *hashtable, void *key, size_t keylen) {
    int index = ht_hash(key, keylen, hashtable->size);

    if (hashtable->arr[index] == NULL) return NULL;

    hashtab_node_t *last_node = hashtable->arr[index];
    while (last_node != NULL) {
        /* only compare matching keylens */
        if (last_node->keylen == keylen) {
            /* compare keys */
            if (memcmp(key, last_node->key, keylen) == 0) {
                return last_node->value;
            }
        }

        last_node = last_node->next;
    }

    return NULL;
}

void *ht_insert(hashtab_t *hashtable, void *key, size_t keylen, void *value,
                size_t vallen) {
    int index = ht_hash(key, keylen, hashtable->size);

    hashtab_node_t *next_node, *last_node;
    next_node = hashtable->arr[index];
    last_node = NULL;

    /* Search for an existing key. */
    while (next_node != NULL) {
        /* only compare matching keylens */
        if (next_node->keylen == keylen) {
            /* compare keys */
            if (memcmp(key, next_node->key, keylen) == 0) {
                /* this key already exists, replace it */
                if (next_node->vallen != vallen) {
                    /* new value is a different size */
                    free(next_node->value);
                    next_node->value = malloc(vallen);
                    if (next_node->value == NULL) return NULL;
                }
                memcpy(next_node->value, value, vallen);
                next_node->vallen = vallen;
                return next_node->value;
            }
        }

        last_node = next_node;
        next_node = next_node->next;
    }

    /* create a new node */
    hashtab_node_t *new_node;
    new_node = (hashtab_node_t *)malloc(sizeof(hashtab_node_t));
    if (new_node == NULL) return NULL;

    /* get some memory for the new node data */
    new_node->key = malloc(keylen);
    new_node->value = malloc(vallen);
    if (new_node->key == NULL || new_node->key == NULL) {
        free(new_node->key);
        free(new_node->value);
        free(new_node);
        return NULL;
    }

    /* copy over the value and key */
    memcpy(new_node->key, key, keylen);
    memcpy(new_node->value, value, vallen);
    new_node->keylen = keylen;
    new_node->vallen = vallen;

    /* no next node */
    new_node->next = NULL;

    /* Tack the new node on the end or right on the table. */
    if (last_node != NULL)
        last_node->next = new_node;
    else
        hashtable->arr[index] = new_node;

    hashtable->count++;
    return new_node->value;
}

/* delete the given key from the hashtable */
void ht_remove(hashtab_t *hashtable, void *key, size_t keylen) {
    hashtab_node_t *last_node, *next_node;
    int index = ht_hash(key, keylen, hashtable->size);
    next_node = hashtable->arr[index];
    last_node = NULL;

    while (next_node != NULL) {
        if (next_node->keylen == keylen) {
            /* compare keys */
            if (memcmp(key, next_node->key, keylen) == 0) {
                /* free node memory */
                free(next_node->value);
                free(next_node->key);

                /* adjust the list pointers */
                if (last_node != NULL)
                    last_node->next = next_node->next;
                else
                    hashtable->arr[index] = next_node->next;

                /* free the node */
                free(next_node);
                break;
            }
        }

        last_node = next_node;
        next_node = next_node->next;
    }
}

/* grow the hashtable */
void *ht_grow(hashtab_t *old_ht, size_t new_size) {
    /* create new hashtable */
    hashtab_t *new_ht = ht_init(new_size, old_ht->hash_func);
    if (new_ht == NULL) return NULL;

    void *ret; /* captures return values */

    /* Iterate through the old hashtable. */
    hashtab_iter_t ii;
    ht_iter_init(old_ht, &ii);
    for (; ii.key != NULL; ht_iter_inc(&ii)) {
        ret = ht_insert(new_ht, ii.key, ii.keylen, ii.value, ii.vallen);
        if (ret == NULL) {
            /* Insert failed. Destroy new hashtable and return. */
            ht_destroy(new_ht);
            return NULL;
        }
    }

    /* Destroy the old hashtable. */
    ht_destroy(old_ht);

    return new_ht;
}

/* free all resources used by the hashtable */
void ht_destroy(hashtab_t *hashtable) {
    hashtab_node_t *next_node, *last_node;

    /* Free each linked list in hashtable. */
    int i;
    for (i = 0; i < (int)hashtable->size; i++) {
        next_node = hashtable->arr[i];
        while (next_node != NULL) {
            /* destroy node */
            free(next_node->key);
            free(next_node->value);
            last_node = next_node;
            next_node = next_node->next;
            free(last_node);
        }
    }

    free(hashtable->arr);
    free(hashtable);
}

/* iterator initilaize */
void ht_iter_init(hashtab_t *hashtable, hashtab_iter_t *ii) {
    /* stick in initial bookeeping data */
    ii->internal.hashtable = hashtable;
    ii->internal.node = NULL;
    ii->internal.index = -1;

    /* have iterator point to first element */
    ht_iter_inc(ii);
}

/* iterator increment  */
void ht_iter_inc(hashtab_iter_t *ii) {
    hashtab_t *hashtable = ii->internal.hashtable;
    int index = ii->internal.index;

    /* attempt to grab the next node */
    if (ii->internal.node == NULL || ii->internal.node->next == NULL)
        index++;
    else {
        /* next node in the list */
        ii->internal.node = ii->internal.node->next;
        ii->key = ii->internal.node->key;
        ii->value = ii->internal.node->value;
        ii->keylen = ii->internal.node->keylen;
        ii->vallen = ii->internal.node->vallen;
        return;
    }

    /* find next node */
    while (hashtable->arr[index] == NULL && index < (int)hashtable->size)
        index++;

    if (index >= (int)hashtable->size) {
        /* end of hashtable */
        ii->internal.node = NULL;
        ii->internal.index = (int)hashtable->size;

        ii->key = NULL;
        ii->value = NULL;
        ii->keylen = 0;
        ii->vallen = 0;
        return;
    }

    /* point to the next item in the hashtable */
    ii->internal.node = hashtable->arr[index];
    ii->internal.index = index;
    ii->key = ii->internal.node->key;
    ii->value = ii->internal.node->value;
    ii->keylen = ii->internal.node->keylen;
    ii->vallen = ii->internal.node->vallen;
}

int ht_hash(void *key, size_t keylen, size_t hashtab_size) {
    int sum = 0;

    /* very simple hash function for now */
    int i;
    for (i = 0; i < (int)keylen; i++) {
        sum += ((unsigned char *)key)[i] * (i + 1);
    }

    return (sum % (int)hashtab_size);
}
#endif
