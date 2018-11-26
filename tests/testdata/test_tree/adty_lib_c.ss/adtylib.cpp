#include "windows.h"

struct Adty_Small_Rect {
	int left;
	int top;
	int right;
	int	bottom;
};

extern "C" {

int
Adty_GetStdHandle (int nStdHandle)
{
	return (int)(GetStdHandle ((DWORD)nStdHandle));
}


int
Adty_SetStdHandle (int nStdHandle, int StdHandle)
{
	return SetStdHandle ((DWORD)nStdHandle, (HANDLE)StdHandle);
}


int
Adty_FillConsoleOutputCharacter (int console, char ch, int length, int x, int y, int* cells_written)
{
	COORD coord = {x, y};

	return FillConsoleOutputCharacter ((HANDLE)console, ch, length, coord, (unsigned long *)cells_written);
}


int
Adty_ReadConsole (int handle, char *buffer, int to_read, int *read)
{
	return ReadConsole ((HANDLE)handle, buffer, to_read, (unsigned long *)read, NULL);
}


int
Adty_WriteConsole (int handle, char *buffer, int to_write, int *written)
{
	return WriteConsole ((HANDLE)handle, buffer, to_write, (unsigned long *)written, NULL);
}


int
Adty_SetConsoleCursorPosition (int handle, int x, int y)
{
	COORD coord = {x, y};

	return SetConsoleCursorPosition ((HANDLE)handle, coord);
}


int
Adty_SetConsoleTextAttribute (int handle, int attribute)
{
	return SetConsoleTextAttribute ((HANDLE)handle, (WORD)attribute);
}


int
Adty_ScrollConsoleScreenBuffer (int handle, struct Adty_Small_Rect *adty_scroll_rect, struct Adty_Small_Rect *adty_clip_rect, int new_x, int new_y, char fill_ch, int attribute)
{
	SMALL_RECT scroll_rect = {adty_scroll_rect->left,
							  adty_scroll_rect->top,
							  adty_scroll_rect->right,
							  adty_scroll_rect->bottom};
	SMALL_RECT clip_rect;
	
	if (adty_clip_rect != NULL)
	{
		clip_rect.Left = adty_clip_rect->left;
		clip_rect.Top = adty_clip_rect->top,
		clip_rect.Right = adty_clip_rect->right,
		clip_rect.Bottom = adty_clip_rect->bottom;
	}

	COORD dest = {new_x, new_y};
	CHAR_INFO fill_char = {fill_ch, attribute};
	
	return ScrollConsoleScreenBuffer ((HANDLE)handle,
									  &scroll_rect,
									  adty_clip_rect != NULL ? &clip_rect : NULL,
									  dest,
									  &fill_char);
}


int
Adty_GetConsoleScreenBufferInfo (int handle, int *cursor_pos_x, int *cursor_pos_y, int *size_x, int *size_y)
{
	CONSOLE_SCREEN_BUFFER_INFO csbi;
	
	BOOL result = GetConsoleScreenBufferInfo ((HANDLE)handle, &csbi);

	*cursor_pos_x = csbi.dwCursorPosition.X;
	*cursor_pos_y = csbi.dwCursorPosition.Y;

	*size_x = csbi.dwSize.X;
	*size_y = csbi.dwSize.Y;

	return result;
}


int
Adty_ReadConsoleInput (int handle, char *ch)
{
	INPUT_RECORD input;
	DWORD records_read;
	int key_input = 0;
	BOOL result;
	int error = 0;

	while (!key_input && !error) {
	   result = ReadConsoleInput ((HANDLE)handle, &input, 1, &records_read);
	  	 
	   error = !result;
	   if (!error) {
		   if (input.EventType == KEY_EVENT && input.Event.KeyEvent.bKeyDown)
			   key_input = 1;
	   }
	}

	if (!error)
	   *ch = input.Event.KeyEvent.uChar.AsciiChar;	

	return result;
}


int
Adty_GetLastError ()
{
	return GetLastError ();
}

}