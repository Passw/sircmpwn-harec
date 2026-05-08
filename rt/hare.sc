SECTIONS {
	.data.hare_init_array : {
		PROVIDE_HIDDEN (__hare_init_array_start = .);
		KEEP (*(.data.hare_init_array))
		PROVIDE_HIDDEN (__hare_init_array_end = .);
	}

	.data.hare_fini_array : {
		PROVIDE_HIDDEN (__hare_fini_array_start = .);
		KEEP (*(.data.hare_fini_array))
		PROVIDE_HIDDEN (__hare_fini_array_end = .);
	}

	.data.hare_test_array : {
		PROVIDE_HIDDEN (__hare_test_array_start = .);
		KEEP (*(.data.hare_test_array))
		PROVIDE_HIDDEN (__hare_test_array_end = .);
	}
} INSERT BEFORE .data
