gen:
	gprbuild -p -c -P gen gen.cpp
	cat .genobj/ialertobject_h.ads .genobj/inotificationlistener_h.ads .genobj/alertproducer_h.ads | \
	sed -e "s-with cpp_4_3_5_stdexcept;--" \
            -e "s-limited with cpp_4_3_5_bits_basic_string_h;--" \
            -e "s-with cpp_4_3_5_bits_stringfwd_h;--"\
            -e "s!-- .*/shared/!--  !" \
            -e "s-cpp_4_3_5_bits_stringfwd_h.string-Extensions.void-"\
            -e "s-cpp_4_3_5_bits_basic_string_h.basic_string-Extensions.void-"\
            -e "s-ialertobject_h-Alarmproducer.Low_Level.ialertobject_h-" \
	    -e "s-inotificationlistener_h-Alarmproducer.Low_Level.inotificationlistener_h-" \
	    -e "s-alertproducer_h-Alarmproducer.Low_Level.alertproducer_h-" >src.ada
	-gnatchop -w src.ada src

