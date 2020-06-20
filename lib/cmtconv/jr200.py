''' Library to read/write National JR-200 tape format.
'''

from __future__ import print_function
import sys

# General approach is to use layered abstractions in
# a simple top-down parser.
# - came out of investigative approach, supports easy debugging
# - not the most efficient
#
# Layers are as follows:
# - waveform -> timed rising/falling edges
# - edges -> mark/space cycles
# - mark/space patterns -> bits
# - bits -> bytes
# - bytes -> file header, blocks


# TODO
#
# take out former code to plot histogram of cycle lengths
#   - useful for investigating other formats
# base cutoff on mean and stddev, instead of/in addition to max/min
# make cutoff a parameter & command line option (?)
# gracefully deal with end of samples and try to read multiple files
#
# Generate file header and blocks from cjr file format
# Generate cjr file from header and blocks
# Generate audio from blocks
# Split out most code into a library
# Add pytest tests
# Documentation
#

def err( *args, **kwargs ):
    print( *args, file = sys.stderr, **kwargs )

def log( *args, **kwargs ):
    print( *args, file = sys.stderr, **kwargs )

def info( *args, **kwargs ):
    print( *args, file = sys.stderr, **kwargs )

def debug( *args, **kwargs ):
    print( *args, file = sys.stderr, **kwargs )

# samples   : [ Float ]
# ->
# levels    : [ Bool ]
def samples_to_levels( samples ):
    if len( samples ) > 0:
        sample_max = max( samples )
        sample_min = min( samples )
        cutoff = sample_min + 0.45 * (sample_max - sample_min)
        debug( 'Max: %d, min: %d, cutoff: %d' %
                ( sample_max, sample_min, cutoff) )
        return [ x > cutoff for x in samples ]
    else:
        return []

# levels        : [ Bool ]
# sample_dur    : Float
# ->
# edges         : [ ( Float, Bool, Float ) ]
#
# The edges returned are a triple of:
# - time
# - rising/falling - rising = True, falling = False
# - pulse length - length of the pulse up to this point
def levels_to_timed_edges( levels, sample_dur ):
    edges = []
    last_t = 0.0
    last_level = levels[ 0 ]
    for (i, l) in enumerate( levels ):
        t = sample_dur * i
        if ( l != last_level ):
            edges.append( (t, l, t - last_t) )
            last_t = t
            last_level = l
    return edges

# edges         : [ ( Float, Bool, Float ) ]
# i_next        : Int
# edges_needed  : Int
# ->
# i_next        : Int
def next_space( edges, i_next, edges_needed ):
    consecutive = 0
    i = i_next
    dur = 1000000 * edges[ i ][ 2 ]
    while i < len( edges ):
        if dur >= 300 and dur <= 500:
            consecutive += 1
            if consecutive > edges_needed:
                # FIXME: below is hack to get some troublesome audio to  work
                # shoud split functionality into 'next_space' and 'read_leader'
                #return i - consecutive
                return i
        i += 1
        dur = 1000000 * edges[ i ][ 2 ]
    raise( Exception( 'unable to find %d consecutive edges of space'
                        % edges_needed))

# edge      : [ ( Float, Bool, Float ) ]
# ->
# result    : Bool
#
# FIXME: in-band error handling
# FIXME: tolerance argument(s)
def is_mark( edge ):
    dur = 1000000 * edge[ 2 ]
    if dur > 150 and dur < 300:
        return 1
    #elif dur > 300 and dur < 500:
    # FIXME: hack to deal with weird pulses at end of block
    elif dur > 300 and dur < 900:
        return 0
    else:
        raise( Exception( 'wavelength out of range: %f (at %f)'
                        % ( dur, edge[ 0 ] ) ) )


# edges         : [ ( Float, Bool, Float ) ]
# i_next        : Int
# ->
# i_next        : Int
def eat_until_mark( edges, i_next ):
    i = i_next
    edge = edges[ i ]
    while not is_mark( edge ):
        i += 1
        edge = edges[ i ]
    return ( i, i - i_next )


# FIXME: below is parameterised over mark/space definition above
#  (i.e. 2400Hz/1200Hz waves)
class decoder( object ):

    # mark_edges    : Int -- number of 2400Hz edges for a mark or '1'
    # space_edges   : Int -- number of 1200Hz edges for a space or '0'
    def __init__( self, mark_edges, space_edges ):
        self.mark_edges     = mark_edges
        self.space_edges    = space_edges

    # edges     : [ ( Float, Bool, Float ) ]
    # idx       : Int
    # ->
    # ( i_next, bit )   : ( Int, Int )
    def eat_bit( self, edges, idx ):
        e = edges[ idx ]
        if is_mark( e ):
            for i in range( self.mark_edges ):
                e = edges[ idx + i ]
                if (not is_mark( e ) ):
                    dur = 1000000 * e[ 2 ]
                    raise( Exception( 'Expected %d mark edges: %f (at %f) '
                        % ( self.mark_edges, dur, e[ 0 ] ) ) )
            return ( idx + self.mark_edges, 1 )
        else:
            for i in range( self.space_edges ):
                e = edges[ idx + i ]
                if ( is_mark( e ) ):
                    dur = 1000000 * e[ 2 ]
                    raise( Exception( 'Expected %d space edges: %f (at %f) '
                        % ( self.space_edges, dur, e[ 0 ] ) ) )
            return ( idx + self.space_edges, 0 )

    # edges     : [ ( Float, Bool, Float ) ]
    # idx       : Int
    # n         : Int
    # ->
    # ( i_next, bits )   : ( Int, [ Int ] )
    def eat_bits( self, edges, idx, n ):
        bits = []
        i_next = idx
        for _ in range( n ):
            ( i_next, bit ) = self.eat_bit( edges, i_next )
            bits.append( bit )
        return ( i_next, bits )

    # Bit pattern for a byte is 1 xxxx xxxx 000

    # edges     : [ ( Float, Bool, Float ) ]
    # i_next    : Int
    # ->
    # i_next    : Int
    def eat_start_bits( self, edges, i_next ):
        ( i_next, bits ) = self.eat_bits( edges, i_next, 1 )
        #debug( bits )
        if( bits == [ 1 ] ):
            return i_next
        else:
            raise( Exception( 'Expected start bits: [ 1 ], got: %s '
                % str( bits ) ) )

    # edges     : [ ( Float, Bool, Float ) ]
    # i_next    : Int
    # ->
    # i_next    : Int
    def eat_stop_bits( self, edges, i_next ):
        ( i_next, bits ) = self.eat_bits( edges, i_next, 3 )
        #debug( bits )
        if( bits == [ 0, 0, 0 ] ):
            return i_next
        else:
            raise( Exception( 'Expected stop bits [ 0, 0, 0 ], got: %s '
                            % str( bits ) ) )

    # We use the definition of mark/1 and space/0 as per the service manual,
    # but the actual bits seem to be inverted. They are also little-endian,
    # with the LSB first.
    #
    # edges     : [ ( Float, Bool, Float ) ]
    # i_nexy    : Int
    # ->
    # ( i_next, res ) : ( Int, Int )
    def eat_raw_byte( self, edges, i_next ):
        ( i_next, bits ) = self.eat_bits( edges, i_next, 8 )
        # debug( bits )
        res = 0
        bits.reverse()
        for b in bits:
            res = 2 * res + ( 1 - b )
        return ( i_next, res )

    # edges     : [ ( Float, Bool, Float ) ]
    # i_next    : Int
    # n         : Int
    # ->
    # ( i_next, res ) : ( Int, [ Int ] )
    def eat_bytes( self, edges, i_next, n ):
        res = []
        for _ in range( n ):
            i_next = self.eat_start_bits( edges, i_next )
            ( i_next, x ) = self.eat_raw_byte( edges, i_next )
            i_next = self.eat_stop_bits( edges, i_next )
            res.append( x )
        return ( i_next, res )


class file_header( object ):
    # Format is as below:
    # 0-1: signature/header - should be 2, 42
    # 2: block number
    # 3: length: 26
    # 4-5: pad: 255
    # 7-21: file name
    # 22: BASIC(0)/Binary(1)
    # 23: Baud rate 2400 - 0, 600 - 1
    # 25-32: pad: 255
    # 33: checksum

    def __init__( self, raw_bytes ):
        b = raw_bytes
        self.raw_bytes      = b
        self.header         = b[ 0:2 ]
        self.block_number   = b[ 2 ]
        self.length         = b[ 3 ]
        self.pad0           = b[ 4:6 ]
        self.filename       = ''.join( chr( x ) for x in b[ 6:22 ] if x > 0 )
        self.filetype       = b[ 22 ]
        self.baud_rate      = b[ 23 ]
        self.pad1           = b[ 24:31 ]
        self.checksum       = b[ 32 ]

    def __str__( self ):
        s = ''
        s += 'JR-200 File header block\n'
        s += 'Header: {}\n'.format( map( hex, self.header ) )
        s += 'Block: {}\n'.format( self.block_number )
        s += 'Length: {}\n'.format( self.length )
        s += 'Pad0: {}\n'.format( map( hex, self.pad0 ) )
        s += 'Filename: "{}"\n'.format( self.filename )
        s += 'File type: {}\n'.format( hex( self.filetype ) )
        s += 'Baud rate: {}\n'.format( hex( self.baud_rate ) )
        s += 'Pad1: {}\n'.format( map( hex, self.pad1 ) )
        s += 'Checksum: {} (calculated: {})\n'.format(
                hex( self.checksum ), hex( sum( self.raw_bytes[:-1] ) % 256 ) )
        return s

    # FIXME: add proper checksum calc to allow checking


class block_header( object ):

    def __init__( self, raw_bytes ):
        b = raw_bytes
        self.raw_bytes      = b
        self.header         = b[ 0:2 ]
        self.block_number   = b[ 2 ]
        self.length         = b[ 3 ]
        self.addr           = b[4] * 256 + b[5]
        if self.header != [ 2, 42 ]:
            raise ( Exception( 'Invalid signature in header: {}'.format(
                                self.header ) ) )

    def __str__( self ):
        s = ''
        s += 'JR-200 Data block header\n'
        s += 'Header: {}\n'.format( map( hex, self.header ) )
        s += 'Block: {}\n'.format( self.block_number )
        s += 'Length: {}\n'.format( self.length )
        s += 'Address: {}\n'.format( self.addr )
        return s

    def is_tail( self ):
        return self.block_number == 255 and self.length == 255

    def calc_checksum( self ):
        if self.is_tail():
            return 0
        else:
            return sum( self.raw_bytes ) % 256

class block( object ):

    def __init__( self, header, bytes, checksum ):
        self.header     = header
        self.data       = bytes
        self.checksum   = checksum

    def __str__( self ):
        s = ''
        s += str( self.header )
        for ( i, x ) in enumerate( self.data ):
            if i % 20 == 0: s+= '\n'
            s += '  {0:02x}'.format( x )
        s += '\n\nChecksum: %x' % self.checksum
        return s

    def calc_checksum( self ):
        if self.header.is_tail():
            return 0
        else:
            header_chksum = self.header.calc_checksum()
            data_chksum = sum( self.data ) % 256
            chksum = (header_chksum + data_chksum) % 256
            debug( 'header_chksum: %d, data_chksum: %d, chksum: %d'
                    % (header_chksum,data_chksum,chksum) )
            return chksum


class file_reader( object ):
    def __init__( self ):
        self.baud600_decoder = decoder( 8, 4 )
        self.baud2400_decoder = decoder( 2, 1 )

    # Read a number of space edges up to the mark for the start-bit of the
    # first byte
    def read_leader( self, edges, i_next ):
        i_next = next_space( edges, i_next, 100 )
        debug( 'Leader cycles detected at %d - %fs (%s)'
            % ( i_next, edges[ i_next ][ 0 ], str( edges[ i_next ][ 1 ]) ) )

        # Read up to the start bit of the first byte
        ( i_next, _ ) = eat_until_mark( edges, i_next )
        debug( 'Start of data at %d - %fs (%s)'
            % ( i_next, edges[ i_next ][ 0 ], edges[ i_next ][ 1 ] ) )

        return i_next

    def read_file_header( self, edges, i_next ):
        i_next = self.read_leader( edges, i_next )

        ( i_next, header_bytes ) = self.baud600_decoder.eat_bytes(
                                    edges, i_next, 33 )
        #debug( header_bytes )
        hdr = file_header( header_bytes )
        debug( hdr )

        debug( 'i_next: %d( %f )' % ( i_next, edges[ i_next ][ 0 ] ) )
        return (i_next, hdr)

    def read_block( self, bit_decoder, edges, i_next ):
        # # Read the leader
        i_next = self.read_leader( edges, i_next )

        # Read the block header
        (i_next, block_header_bytes) = bit_decoder.eat_bytes(edges, i_next, 6)
        block_hdr = block_header( block_header_bytes )
        #debug( block_hdr )
        if block_hdr.is_tail():
            return ( i_next, block( block_hdr, [], 0 ) )
        else:
            # Read the data
            actual_length = block_hdr.length if block_hdr.length > 0 else 256
            ( i_next, block_data_bytes ) = bit_decoder.eat_bytes(
                                             edges, i_next, actual_length + 1 )
            block_data_chksum = block_data_bytes[-1]
            block_data_bytes.pop()

            blk = block( block_hdr, block_data_bytes, block_data_chksum )
            # debug( blk )
            debug( 'i_next: %d( %f )' % ( i_next, edges[ i_next ][ 0 ] ) )
            return ( i_next, blk )

    # read blocks
    # returns ( int, [block] )
    def read_blocks( self, bit_decoder, edges, i_next ):
        blocks = []

        while True:
            ( i_next, blk ) = self.read_block( bit_decoder, edges, i_next )
            blocks.append( blk )
            debug( blk )
            if blk.header.is_tail():
                break

        return (i_next, blocks)

    # read a file header and all blocks
    # returns ( int, ( file_header, [ block ] ) )
    def read_file( self, edges, i_next ):
        ( i_next, file_hdr ) = self.read_file_header( edges, i_next )
        if file_hdr.baud_rate == 0:
            bit_decoder = self.baud2400_decoder
        else:
            bit_decoder = self.baud600_decoder
        ( i_next, blocks ) = self.read_blocks( bit_decoder, edges, i_next )
        return (i_next, ( file_hdr, blocks ) )

    # FIXME: read_files - return [ ( file_header, [block] ) ]


# Check checksums are valid
#
# blocks : [ block ]
# ->
# Bool
def checksums_valid( blocks ):
    for block in blocks:
        chksum = block.calc_checksum()
        if ( block.checksum != chksum ):
            err('Block %d has checksum %d, calculated checksum is %d'
                % (block.header.block_number, block.checksum, chksum))
            return False
    return True

# Convert blocks to bytes
#
# blocks : [ block ]
# ->
# bytearray
def blocks_to_bytes( blocks ):
    res = bytearray()
    for block in blocks:
        res.extend( block.data )
    return res

