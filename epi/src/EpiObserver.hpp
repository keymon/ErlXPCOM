/*
***** BEGIN LICENSE BLOCK *****

This file is part of the EPI (Erlang Plus Interface) Library.

Copyright (C) 2005 Hector Rivas Gandara <keymon@gmail.com>

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

***** END LICENSE BLOCK *****
*/

#ifndef __EPIOBSERVER_HPP
#define __EPIOBSERVER_HPP

#include <set>

namespace epi {
namespace node {

class EpiObserver;

enum EpiEventTag {
    EVENT_DESTROY
};

class EpiObservable {
    typedef std::set<EpiObserver*> observers_set;
public:

    void addObserver(EpiObserver *observer);

    void notify(EpiEventTag event);

    void removeObserver(EpiObserver *observer);
private:
    observers_set mObservers;
};


class EpiObserver {
public:

    /**
     * Receive an event
     */
    virtual void event(EpiObservable* observed, EpiEventTag event) = 0;
};


} // namespace node
} // namespace epi


#endif // __EPIOBSERVER_HPP
