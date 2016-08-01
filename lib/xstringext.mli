(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
module String :
  sig
    include module type of String

    val of_char : char -> string

    (** Make a string of the given length with characters generated by the
	given function. *)
    val init : int -> (int -> char) -> string

    (** Map a string to a string. *)
    val map : (char -> char) -> string -> string

    (** Map a string to a string, applying the given function in reverse
	order. *)
    val rev_map : (char -> char) -> string -> string

    (** Iterate over the characters in a string in reverse order. *)
    val rev_iter : (char -> 'a) -> string -> unit

    (** Fold over the characters in a string. *)
    val fold_left : ('a -> char -> 'a) -> 'a -> string -> 'a

    (** Iterate over the characters with the character index in argument *)
    val iteri : (int -> char -> 'a) -> string -> unit

    (** Iterate over the characters in a string in reverse order. *)
    val fold_right : (char -> 'a -> 'a) -> string -> 'a -> 'a

    (** Split a string into a list of characters. *)
    val explode : string -> char list

    (** Concatenate a list of characters into a string. *)
    val implode : char list -> string

    (** True if string 'x' ends with suffix 'suffix' *)
    val endswith : string -> string -> bool

    (** True if string 'x' starts with prefix 'prefix' *)
    val startswith : string -> string -> bool

    (** True if the character is whitespace *)
    val isspace : char -> bool

    (** Removes all the characters from the ends of a string for which the predicate is true *)
    val strip : (char -> bool) -> string -> string

    (** Backward-compatible string escaping, defaulting to the built-in
	OCaml string escaping but allowing an arbitrary mapping from characters
	to strings. *)
    val escaped : ?rules:(char * string) list -> string -> string

    (** Take a predicate and a string, return a list of strings separated by
	runs of characters where the predicate was true *)
    val split_f : (char -> bool) -> string -> string list

    (** split a string on a single char *)
    val split : ?limit:int -> char -> string -> string list

    (** FIXME document me|remove me if similar to strip *)
    val rtrim : string -> string

    (** True if sub is a substr of str *)
    val has_substr : string -> string -> bool

(** find all occurences of needle in haystack and return all their respective index *)
    val find_all : string -> string -> int list

    (** replace all [f] substring in [s] by [t] *)
    val replace : string -> string -> string -> string

    (** filter chars from a string *)
    val filter_chars : string -> (char -> bool) -> string

    (** map a string trying to fill the buffer by chunk *)
    val map_unlikely : string -> (char -> string option) -> string

    (** a substring from the specified position to the end of the string *)
    val sub_to_end : string -> int -> string
    
    (** a substring from the start of the string to the first occurrence of a given character, excluding the character *)
    val sub_before : char -> string -> string
    
    (** a substring from  the first occurrence of a given character to the end of the string, excluding the character *)
    val sub_after : char -> string -> string
  end
