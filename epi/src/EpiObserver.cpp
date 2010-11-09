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


#include "EpiObserver.hpp"

using namespace epi::node;

void EpiObservable::addObserver(EpiObserver *observer) {
    mObservers.insert(observer);
}

void EpiObservable::notify(EpiEventTag event) {
    for (observers_set::iterator i = mObservers.begin(); i != mObservers.end(); i++) {
        (*i)->event(this, event);
    }
}

void EpiObservable::removeObserver(EpiObserver *observer) {
    mObservers.erase(observer);
}
