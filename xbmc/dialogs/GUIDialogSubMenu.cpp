/*
 *      Copyright (C) 2005-2012 Team XBMC
 *      http://www.xbmc.org
 *
 *  This Program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This Program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XBMC; see the file COPYING.  If not, write to
 *  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston, MA 02110, USA.
 *  http://www.gnu.org/copyleft/gpl.html
 *
 */

#include "GUIDialogSubMenu.h"

CGUIDialogSubMenu::CGUIDialogSubMenu(int id, const CStdString &xmlFile)
    : CGUIDialog(id, xmlFile)
{
}

CGUIDialogSubMenu::~CGUIDialogSubMenu(void)
{}

bool CGUIDialogSubMenu::OnMessage(CGUIMessage &message)
{
  if (message.GetMessage() == GUI_MSG_CLICKED)
  {
    // someone has been clicked - deinit...
    CGUIDialog::OnMessage(message);
    Close();
    return true;
  }
  return CGUIDialog::OnMessage(message);
}
