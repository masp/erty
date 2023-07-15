Using patterns in Erlang is extremely powerful

Reusing complex patterns and combining them is impossible without the use of macros. For example, say you have a common byte header that you want to share between multiple programs:

```golang
func read_header(payload binary) {Header, binary} {
    <<?IP_VERSION:4, HLen:4, Rest/bytes>> := payload
    

     <<?IP_VERSION:4, HLen:4, SrvcType:8, TotLen:16, 
      ID:16, Flgs:3, FragOff:13,
      TTL:8, Proto:8, HdrChkSum:16,
      SrcIP:32,
      DestIP:32, RestDgram/binary>> when HLen>=5, 4*HLen=<DgramSize ->
        OptsLen = 4*(HLen - ?IP_MIN_HDR_LEN),
        <<Opts:OptsLen/binary,Data/binary>> = RestDgram,

    []bit{bits(10)..., 
    []byte{
        byte(A, ['unsigned', 'big']), }
    []bit{1, 0, 1}
    [sz:8/unsigned big integer]
    <<sz:8, payload:((sz-1)*8)/binary, rest/binary>> := payload
    return {Header{size: sz, payload: payload}, rest}
}

func parse_packet(p binary) {Packet, binary} {
    {header, rest} := header(p)
    {body, rest} := body(rest)

}
```