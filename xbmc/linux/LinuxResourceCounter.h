#ifndef __LINUX_RESOURCE_COUNTER__H__
#define __LINUX_RESOURCE_COUNTER__H__

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

#include <sys/time.h>
#include <sys/times.h>
#include <sys/resource.h>

#include <sys/time.h>
#include <time.h>

class CLinuxResourceCounter
{
public:
  CLinuxResourceCounter();
  virtual ~CLinuxResourceCounter();

  double GetCPUUsage();
  void   Reset();

protected:
  struct rusage  m_usage;
  struct timeval m_tmLastCheck;
  double m_dLastUsage;
};

#endif

