package runtime

import "unsafe"

func mapaccess1_fast64(t *maptype, h *hmap, key uint64) unsafe.Pointer {
	if raceenabled && h != nil {
		callerpc := getcallerpc()
		racereadpc(unsafe.Pointer(h), callerpc, funcPC(mapaccess1_fast64))
	}
	if h == nil || h.count == 0 {
		return unsafe.Pointer(&zeroVal[0])
	}
	if h.flags&hashWriting != 0 {
		throw("concurrent map read and map write")
	}
	var b *bmap
	if h.B == 0 {
		// One-bucket table. No need to hash.
		b = (*bmap)(h.buckets)
	} else {
		hash := t.key.alg.hash(noescape(unsafe.Pointer(&key)), uintptr(h.hash0))
		m := bucketMask(h.B)
		b = (*bmap)(add(h.buckets, (hash&m)*uintptr(t.bucketsize)))
		if c := h.oldbuckets; c != nil {
			if !h.sameSizeGrow() {
				// There used to be half as many buckets; mask down one more power of two.
				m >>= 1
			}
			oldb := (*bmap)(add(c, (hash&m)*uintptr(t.bucketsize)))
			if !evacuated(oldb) {
				b = oldb
			}
		}
	}
	for ; b != nil; b = b.overflow(t) {
		for i, k := uintptr(0), b.keys(); i < bucketCnt; i, k = i+1, add(k, 8) {
			if *(*uint64)(k) == key && b.tophash[i] != empty {
				return add(unsafe.Pointer(b), dataOffset+bucketCnt*8+i*uintptr(t.valuesize))
			}
		}
	}
	return unsafe.Pointer(&zeroVal[0])
}

func mapaccess2_fast64(t *maptype, h *hmap, key uint64) (unsafe.Pointer, bool) {
	if raceenabled && h != nil {
		callerpc := getcallerpc()
		racereadpc(unsafe.Pointer(h), callerpc, funcPC(mapaccess2_fast64))
	}
	if h == nil || h.count == 0 {
		return unsafe.Pointer(&zeroVal[0]), false
	}
	if h.flags&hashWriting != 0 {
		throw("concurrent map read and map write")
	}
	var b *bmap
	if h.B == 0 {
		// One-bucket table. No need to hash.
		b = (*bmap)(h.buckets)
	} else {
		hash := t.key.alg.hash(noescape(unsafe.Pointer(&key)), uintptr(h.hash0))
		m := bucketMask(h.B)
		b = (*bmap)(add(h.buckets, (hash&m)*uintptr(t.bucketsize)))
		if c := h.oldbuckets; c != nil {
			if !h.sameSizeGrow() {
				// There used to be half as many buckets; mask down one more power of two.
				m >>= 1
			}
			oldb := (*bmap)(add(c, (hash&m)*uintptr(t.bucketsize)))
			if !evacuated(oldb) {
				b = oldb
			}
		}
	}
	for ; b != nil; b = b.overflow(t) {
		for i, k := uintptr(0), b.keys(); i < bucketCnt; i, k = i+1, add(k, 8) {
			if *(*uint64)(k) == key && b.tophash[i] != empty {
				return add(unsafe.Pointer(b), dataOffset+bucketCnt*8+i*uintptr(t.valuesize)), true
			}
		}
	}
	return unsafe.Pointer(&zeroVal[0]), false
}

func mapassign_fast64(t *maptype, h *hmap, key uint64) unsafe.Pointer {
	if h == nil {
		panic(plainError("assignment to entry in nil map"))
	}
	if raceenabled {
		callerpc := getcallerpc()
		racewritepc(unsafe.Pointer(h), callerpc, funcPC(mapassign_fast64))
	}
	if h.flags&hashWriting != 0 {
		throw("concurrent map writes")
	}
	hash := t.key.alg.hash(noescape(unsafe.Pointer(&key)), uintptr(h.hash0))

	// Set hashWriting after calling alg.hash for consistency with mapassign.
	h.flags |= hashWriting

	if h.buckets == nil {
		h.buckets = newobject(t.bucket) // newarray(t.bucket, 1)
	}

again:
	bucket := hash & bucketMask(h.B)
	if h.growing() {
		growWork_fast64(t, h, bucket)
	}
	b := (*bmap)(unsafe.Pointer(uintptr(h.buckets) + bucket*uintptr(t.bucketsize)))

	var insertb *bmap
	var inserti uintptr
	var insertk unsafe.Pointer

	for {
		for i := uintptr(0); i < bucketCnt; i++ {
			if b.tophash[i] == empty {
				if insertb == nil {
					insertb = b
					inserti = i
				}
				continue
			}
			k := *((*uint64)(add(unsafe.Pointer(b), dataOffset+i*8)))
			if k != key {
				continue
			}
			insertb = b
			inserti = i
			goto done
		}
		ovf := b.overflow(t)
		if ovf == nil {
			break
		}
		b = ovf
	}

	// Did not find mapping for key. Allocate new cell & add entry.

	// If we hit the max load factor or we have too many overflow buckets,
	// and we're not already in the middle of growing, start growing.
	if !h.growing() && (overLoadFactor(h.count+1, h.B) || tooManyOverflowBuckets(h.noverflow, h.B)) {
		hashGrow(t, h)
		goto again // Growing the table invalidates everything, so try again
	}

	if insertb == nil {
		// all current buckets are full, allocate a new one.
		insertb = h.newoverflow(t, b)
		inserti = 0 // not necessary, but avoids needlessly spilling inserti
	}
	insertb.tophash[inserti&(bucketCnt-1)] = tophash(hash) // mask inserti to avoid bounds checks

	insertk = add(unsafe.Pointer(insertb), dataOffset+inserti*8)
	// store new key at insert position
	if t.key.kind&kindNoPointers == 0 && writeBarrier.enabled {
		if sys.PtrSize == 8 {
			writebarrierptr((*uintptr)(insertk), uintptr(key))
		} else {
			// There are three ways to squeeze at least one 32 bit pointer into 64 bits.
			// Give up and call typedmemmove.
			typedmemmove(t.key, insertk, unsafe.Pointer(&key))
		}
	} else {
		*(*uint64)(insertk) = key
	}

	h.count++
done:
	val := add(unsafe.Pointer(insertb), dataOffset+bucketCnt*8+inserti*uintptr(t.valuesize))
	if h.flags&hashWriting == 0 {
		throw("concurrent map writes")
	}
	h.flags &^= hashWriting
	return val
}

func mapdelete_fast64(t *maptype, h *hmap, key uint64) {
	if raceenabled && h != nil {
		callerpc := getcallerpc()
		racewritepc(unsafe.Pointer(h), callerpc, funcPC(mapdelete_fast64))
	}
	if h == nil || h.count == 0 {
		return
	}
	if h.flags&hashWriting != 0 {
		throw("concurrent map writes")
	}

	hash := t.key.alg.hash(noescape(unsafe.Pointer(&key)), uintptr(h.hash0))

	// Set hashWriting after calling alg.hash for consistency with mapdelete
	h.flags |= hashWriting

	bucket := hash & bucketMask(h.B)
	if h.growing() {
		growWork_fast64(t, h, bucket)
	}
	b := (*bmap)(add(h.buckets, bucket*uintptr(t.bucketsize)))
search:
	for ; b != nil; b = b.overflow(t) {
		for i, k := uintptr(0), b.keys(); i < bucketCnt; i, k = i+1, add(k, 8) {
			if key != *(*uint64)(k) || b.tophash[i] == empty {
				continue
			}
			// Only clear key if there are pointers in it.
			if t.key.kind&kindNoPointers == 0 {
				memclrHasPointers(k, t.key.size)
			}
			// Only clear value if there are pointers in it.
			if t.elem.kind&kindNoPointers == 0 {
				v := add(unsafe.Pointer(b), dataOffset+bucketCnt*8+i*uintptr(t.valuesize))
				memclrHasPointers(v, t.elem.size)
			}
			b.tophash[i] = empty
			h.count--
			break search
		}
	}

	if h.flags&hashWriting == 0 {
		throw("concurrent map writes")
	}
	h.flags &^= hashWriting
}

func growWork_fast64(t *maptype, h *hmap, bucket uintptr) {
	// make sure we evacuate the oldbucket corresponding
	// to the bucket we're about to use
	evacuate_fast64(t, h, bucket&h.oldbucketmask())

	// evacuate one more oldbucket to make progress on growing
	if h.growing() {
		evacuate_fast64(t, h, h.nevacuate)
	}
}

func evacuate_fast64(t *maptype, h *hmap, oldbucket uintptr) {
	b := (*bmap)(add(h.oldbuckets, oldbucket*uintptr(t.bucketsize)))
	newbit := h.noldbuckets()
	if !evacuated(b) {
		// TODO: reuse overflow buckets instead of using new ones, if there
		// is no iterator using the old buckets.  (If !oldIterator.)

		// xy contains the x and y (low and high) evacuation destinations.
		var xy [2]evacDst
		x := &xy[0]
		x.b = (*bmap)(add(h.buckets, oldbucket*uintptr(t.bucketsize)))
		x.k = add(unsafe.Pointer(x.b), dataOffset)
		x.v = add(x.k, bucketCnt*8)

		if !h.sameSizeGrow() {
			// Only calculate y pointers if we're growing bigger.
			// Otherwise GC can see bad pointers.
			y := &xy[1]
			y.b = (*bmap)(add(h.buckets, (oldbucket+newbit)*uintptr(t.bucketsize)))
			y.k = add(unsafe.Pointer(y.b), dataOffset)
			y.v = add(y.k, bucketCnt*8)
		}

		for ; b != nil; b = b.overflow(t) {
			k := add(unsafe.Pointer(b), dataOffset)
			v := add(k, bucketCnt*8)
			for i := 0; i < bucketCnt; i, k, v = i+1, add(k, 8), add(v, uintptr(t.valuesize)) {
				top := b.tophash[i]
				if top == empty {
					b.tophash[i] = evacuatedEmpty
					continue
				}
				if top < minTopHash {
					throw("bad map state")
				}
				var useY uint8
				if !h.sameSizeGrow() {
					// Compute hash to make our evacuation decision (whether we need
					// to send this key/value to bucket x or bucket y).
					hash := t.key.alg.hash(k, uintptr(h.hash0))
					if hash&newbit != 0 {
						useY = 1
					}
				}

				b.tophash[i] = evacuatedX + useY // evacuatedX + 1 == evacuatedY, enforced in makemap
				dst := &xy[useY]                 // evacuation destination

				if dst.i == bucketCnt {
					dst.b = h.newoverflow(t, dst.b)
					dst.i = 0
					dst.k = add(unsafe.Pointer(dst.b), dataOffset)
					dst.v = add(dst.k, bucketCnt*8)
				}
				dst.b.tophash[dst.i&(bucketCnt-1)] = top // mask dst.i as an optimization, to avoid a bounds check

				// Copy key.
				if t.key.kind&kindNoPointers == 0 && writeBarrier.enabled {
					if sys.PtrSize == 8 {
						writebarrierptr((*uintptr)(dst.k), *(*uintptr)(k))
					} else {
						// There are three ways to squeeze at least one 32 bit pointer into 64 bits.
						// Give up and call typedmemmove.
						typedmemmove(t.key, dst.k, k)
					}
				} else {
					*(*uint64)(dst.k) = *(*uint64)(k)
				}

				typedmemmove(t.elem, dst.v, v)
				dst.i++
				// These updates might push these pointers past the end of the
				// key or value arrays.  That's ok, as we have the overflow pointer
				// at the end of the bucket to protect against pointing past the
				// end of the bucket.
				dst.k = add(dst.k, 8)
				dst.v = add(dst.v, uintptr(t.valuesize))
			}
		}
		// Unlink the overflow buckets & clear key/value to help GC.
		if h.flags&oldIterator == 0 && t.bucket.kind&kindNoPointers == 0 {
			b := add(h.oldbuckets, oldbucket*uintptr(t.bucketsize))
			// Preserve b.tophash because the evacuation
			// state is maintained there.
			ptr := add(b, dataOffset)
			n := uintptr(t.bucketsize) - dataOffset
			memclrHasPointers(ptr, n)
		}
	}

	if oldbucket == h.nevacuate {
		advanceEvacuationMark(h, t, newbit)
	}
}

func growWork_faststr(t *maptype, h *hmap, bucket uintptr) {
	// make sure we evacuate the oldbucket corresponding
	// to the bucket we're about to use
	evacuate_faststr(t, h, bucket&h.oldbucketmask())

	// evacuate one more oldbucket to make progress on growing
	if h.growing() {
		evacuate_faststr(t, h, h.nevacuate)
	}
}

func evacuate_faststr(t *maptype, h *hmap, oldbucket uintptr) {
	b := (*bmap)(add(h.oldbuckets, oldbucket*uintptr(t.bucketsize)))
	newbit := h.noldbuckets()
	if !evacuated(b) {
		// TODO: reuse overflow buckets instead of using new ones, if there
		// is no iterator using the old buckets.  (If !oldIterator.)

		// xy contains the x and y (low and high) evacuation destinations.
		var xy [2]evacDst
		x := &xy[0]
		x.b = (*bmap)(add(h.buckets, oldbucket*uintptr(t.bucketsize)))
		x.k = add(unsafe.Pointer(x.b), dataOffset)
		x.v = add(x.k, bucketCnt*2*sys.PtrSize)

		if !h.sameSizeGrow() {
			// Only calculate y pointers if we're growing bigger.
			// Otherwise GC can see bad pointers.
			y := &xy[1]
			y.b = (*bmap)(add(h.buckets, (oldbucket+newbit)*uintptr(t.bucketsize)))
			y.k = add(unsafe.Pointer(y.b), dataOffset)
			y.v = add(y.k, bucketCnt*2*sys.PtrSize)
		}

		for ; b != nil; b = b.overflow(t) {
			k := add(unsafe.Pointer(b), dataOffset)
			v := add(k, bucketCnt*2*sys.PtrSize)
			for i := 0; i < bucketCnt; i, k, v = i+1, add(k, 2*sys.PtrSize), add(v, uintptr(t.valuesize)) {
				top := b.tophash[i]
				if top == empty {
					b.tophash[i] = evacuatedEmpty
					continue
				}
				if top < minTopHash {
					throw("bad map state")
				}
				var useY uint8
				if !h.sameSizeGrow() {
					// Compute hash to make our evacuation decision (whether we need
					// to send this key/value to bucket x or bucket y).
					hash := t.key.alg.hash(k, uintptr(h.hash0))
					if hash&newbit != 0 {
						useY = 1
					}
				}

				b.tophash[i] = evacuatedX + useY // evacuatedX + 1 == evacuatedY, enforced in makemap
				dst := &xy[useY]                 // evacuation destination

				if dst.i == bucketCnt {
					dst.b = h.newoverflow(t, dst.b)
					dst.i = 0
					dst.k = add(unsafe.Pointer(dst.b), dataOffset)
					dst.v = add(dst.k, bucketCnt*2*sys.PtrSize)
				}
				dst.b.tophash[dst.i&(bucketCnt-1)] = top // mask dst.i as an optimization, to avoid a bounds check

				// Copy key.
				*(*string)(dst.k) = *(*string)(k)

				typedmemmove(t.elem, dst.v, v)
				dst.i++
				// These updates might push these pointers past the end of the
				// key or value arrays.  That's ok, as we have the overflow pointer
				// at the end of the bucket to protect against pointing past the
				// end of the bucket.
				dst.k = add(dst.k, 2*sys.PtrSize)
				dst.v = add(dst.v, uintptr(t.valuesize))
			}
		}
		// Unlink the overflow buckets & clear key/value to help GC.
		// Unlink the overflow buckets & clear key/value to help GC.
		if h.flags&oldIterator == 0 && t.bucket.kind&kindNoPointers == 0 {
			b := add(h.oldbuckets, oldbucket*uintptr(t.bucketsize))
			// Preserve b.tophash because the evacuation
			// state is maintained there.
			ptr := add(b, dataOffset)
			n := uintptr(t.bucketsize) - dataOffset
			memclrHasPointers(ptr, n)
		}
	}

	if oldbucket == h.nevacuate {
		advanceEvacuationMark(h, t, newbit)
	}
}
