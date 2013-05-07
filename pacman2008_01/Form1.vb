Public Class Form1
    'dasar : antar gambar di gambarkan 32 pixel, dan semua labirin(maze) didasarkan pada ini

    'walaupun pada gambarnya berukuran 33x33, alasan menggunakan gambar berukuran 32 pixel adalah karena 
    'lebih mudah dibagi. perpindahan birdman dan musuhnya adalah dasar setengah atau seperempat ukuran dari ikon

    Inherits System.Windows.Forms.Form

    'Deklarasi fungsi yang akan digunakan dalam pembuatan labirin/tembok birdman
    Declare Function GetAsyncKeyState Lib "user32" (ByVal vKey As Integer) As Short


    Const VK_ESC = &H1B
    Dim blockxPosition As Integer = 0   'posisi
    Dim direction As String             'arah
    Dim keypressed As String            'tombolkunci

    Private bigdots() As PictureBox = {} 'dots
    Private pict() As PictureBox = {}    'maze blocks

    Private Sub TextBox_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs)
        ' Display the current text.
        Dim txt As TextBox = DirectCast(sender, TextBox)
        Debug.WriteLine(txt.Name & ": [" & txt.Text & "]")
    End Sub

    Private Sub Form1_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles Me.KeyDown
        WhichKey = e.KeyCode 'Kode kunci
        'Label1.Text = WhichKey
        TurningSub()
        If ShouldExit = True Then
            ShouldExit = False
            Exit Sub
        End If
        If e.KeyCode = 39 Then 'kanan
            If turning = True And PacMan.Top Mod 32 <> 0 Then Exit Sub
            Timer2.Enabled = False
            Timer3.Enabled = False
            Timer4.Enabled = False
            Timer1.Enabled = True
        End If
        If e.KeyCode = 37 Then 'kiri
            If turning = True And PacMan.Top Mod 32 <> 0 Then Exit Sub
            Timer1.Enabled = False
            Timer3.Enabled = False
            Timer4.Enabled = False
            Timer2.Enabled = True
        End If
        If e.KeyCode = 40 Then  'bawah
            If turning = True And PacMan.Left Mod 32 <> 0 Then Exit Sub
            Timer1.Enabled = False
            Timer2.Enabled = False
            Timer4.Enabled = False
            Timer3.Enabled = True
        End If

        If e.KeyCode = 38 Then 'atas
            If turning = True And PacMan.Left Mod 32 <> 0 Then Exit Sub
            Timer1.Enabled = False
            Timer2.Enabled = False
            Timer3.Enabled = False
            Timer4.Enabled = True
        End If

        If e.KeyCode = VK_ESC Then End
    End Sub

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Dim i As Integer
        For i = 0 To 317
            i = pict.Length
            i = bigdots.Length
            ReDim Preserve pict(i)
            ReDim Preserve bigdots(i)
            pict(i) = New PictureBox
            bigdots(i) = New PictureBox
            pict(i).SizeMode = PictureBoxSizeMode.AutoSize
            bigdots(i).SizeMode = PictureBoxSizeMode.AutoSize
            pict(i).Image = PictureBox1.Image
            bigdots(i).Image = dot03.Image
            pict(i).Visible = False
            bigdots(i).Visible = False
            pict(i).Name = "TextBox" & i.ToString()
            bigdots(i).Name = "TextBox" & i.ToString()
            If pict.Length > 1 Then
                pict(i).Left = pict(i - 1).Left
                bigdots(i).Left = bigdots(i - 1).Left
                pict(i).Top = pict(i - 1).Top + pict(i - 1).Height + 4
                bigdots(i).Top = bigdots(i - 1).Top + bigdots(i - 1).Height + 4
                pict(i).Size = pict(i - 1).Size
                bigdots(i).Size = bigdots(i - 1).Size
            End If
            pict(i).Tag = i
            bigdots(i).Tag = i
            AddHandler pict(i).TextChanged, AddressOf TextBox_TextChanged
            AddHandler bigdots(i).TextChanged, AddressOf TextBox_TextChanged
            Me.Controls.Add(pict(i))
            Me.Controls.Add(bigdots(i))
        Next i
        reset()
        loadMaze()


    End Sub
    Sub loadMaze()
        turning = False
        PacManRow = 2           'baris yang akan dilewati oleh Birdman
        PacManColumn = 2        'kolom yang akan dilewati oleh Birdman
        Dim a(13) As String
        Dim count As Integer
        Dim x As Integer
        Dim i, j, y, rows, columns, movedown, moveacross As Integer
        rows = 14
        columns = 20
        first = first + 1
        If first > 3 Then first = 3
        'first = 3
        If first = 1 Then
            a(1) = "1111111111111111111"   ''maze(labirin) pertama yang diload/diproses
            a(2) = "1222222222212222231"
            a(3) = "1211121111222111121"
            a(4) = "1211121111112111121"
            a(5) = "1211121000012111121"
            a(6) = "1222221000012111121"
            a(7) = "1211121000012111121"
            a(8) = "1211121111112222221"
            a(9) = "1211122222222111121"
            a(10) = "1222112111112111121"
            a(11) = "1212112111112111121"
            a(12) = "1322222222222222231"
            a(13) = "1111111111111111111"
        End If
        If first = 2 Then
            a(1) = "1111111111111111111"
            a(2) = "1222221111112222231"
            a(3) = "1211122222222111121"
            a(4) = "1211121111112111221"
            a(5) = "1211121000012111211"
            a(6) = "1222221000012222211"
            a(7) = "1211121000012111211"
            a(8) = "1222221111112222221"
            a(9) = "1211122222222111121"
            a(10) = "1211112111112111121"
            a(11) = "1211112111112111121"
            a(12) = "1322222222222222231"
            a(13) = "1111111111111111111"
        End If
        If first = 3 Then
            a(1) = "1111111111111111111"
            a(2) = "1222222122212222231"
            a(3) = "1211212221222111121"
            a(4) = "1221221111112121221"
            a(5) = "1121121000012121211"
            a(6) = "1222221000012222221"
            a(7) = "1121121000012111211"
            a(8) = "1222221111112222221"
            a(9) = "1211122222222111121"
            a(10) = "1212212121212122221"
            a(11) = "1212112121212121121"
            a(12) = "1322222221222222231"
            a(13) = "1111111111111111111"
        End If

        For count = 1 To 20
            For x = 1 To 13
                maze(x, count) = Mid(a(x), count, 1)
            Next x
        Next count

        movedown = pict(0).Height
        moveacross = pict(0).Width
        pict(0).Visible = False
        bigdots(0).Visible = 0
        For i = 0 To rows
            For j = 0 To columns
                y = y + 1
                If first > 1 Then pict(y).Visible = 0 ''digunakan untuk menghapus block dari maze yang sebelumnya sudah dilihat/dilewati oleh birdman 
                If maze(i, j) = "1" Then                  'memberikan efek yang baik untuk block yang telah dihapus (agar tak nampak)
                    If first = 1 Then pict(y).Image = Wall.Image
                    pict(y).Top = movedown * (i - 1)
                    pict(y).Left = movedown * (j - 1)
                    pict(y).Visible = 1
                End If   'baris selanjutnya mengizinkan maze berikutnya untuk menempatkan block dimanapun
                If maze(i, j) <> "1" And first = 1 Then pict(y).Image = Wall.Image
                If maze(i, j) = "2" Then
                    If first = 1 Then
                        bigdots(y).Visible = 1
                    End If
                    bigdots(y).Top = movedown * (i - 1)
                    bigdots(y).Left = movedown * (j - 1)
                    bigdots(y).Visible = 1
                    bead(i, j) = y
                End If
                If maze(i, j) <> "2" And maze(i, j) <> "3" And first = 1 Then bigdots(i).Image = dot05.Image
                If maze(i, j) = "3" Then
                    If first = 1 Then bigdots(i).Image = dot05.Image
                    bigdots(y).Image = dot05.Image
                    bigdots(y).Top = movedown * (i - 1)
                    bigdots(y).Left = movedown * (j - 1)
                    bigdots(y).Visible = 1
                    bead(i, j) = y
                End If
            Next j
        Next i
        AxWindowsMediaPlayer1.URL = "Maryhay.mid"
    End Sub

    Sub BADGUYS1CONTROL()  'jika terjebak atau terhalang oleh labirin, maka BADman akan berhenti terlebih dahulu untuk melihat jalan mana yang bisa dilewati agar dia bisa mengubah arah, 
        'kemudian jika memungkinkan, BAdman akan mengambil arah yang menuju pacman.
        Dim direct As Integer
        y1 = PacMan.Top - BadMan1.Top   'untuk melihat seberapa jauh jarak atas-bawah birdman menuju BADman
        x1 = PacMan.Left - BadMan1.Left  'untuk melihat seberapa jauh jarak kiri-kanan birdman menuju BADman

        If maze(BADman1ROW + 1, BADman1COLUMN) = "1" And maze(BADman1ROW - 1, BADman1COLUMN) = "1" And maze(BADman1ROW, BADman1COLUMN + 1) = "1" Then
            Timer8.Enabled = False  'buntu ke arah kanan maka BADman akan ke kiri
            Timer7.Enabled = False
            Timer5.Enabled = True
            Timer6.Enabled = False
            Exit Sub
        End If
        If maze(BADman1ROW + 1, BADman1COLUMN) = "1" And maze(BADman1ROW - 1, BADman1COLUMN) = "1" And maze(BADman1ROW, BADman1COLUMN - 1) = "1" Then
            Timer8.Enabled = False  'buntu ke arah kiri maka BADman akan ke kanan
            Timer7.Enabled = False
            Timer5.Enabled = False
            Timer6.Enabled = True
            Exit Sub
        End If
        If maze(BADman1ROW, BADman1COLUMN + 1) = "1" And maze(BADman1ROW, BADman1COLUMN - 1) = "1" And maze(BADman1ROW + 1, BADman1COLUMN) = "1" Then
            Timer8.Enabled = False  'jika buntu bawahnya, maka BADman akan langsung mengubah arah menjadi naik ketas
            Timer7.Enabled = True
            Timer5.Enabled = False
            Timer6.Enabled = False
            Exit Sub
        End If
        If maze(BADman1ROW, BADman1COLUMN + 1) = "1" And maze(BADman1ROW, BADman1COLUMN - 1) = "1" And maze(BADman1ROW - 1, BADman1COLUMN) = "1" Then
            Timer8.Enabled = True  'jika buntu atasnya, maka BADdman akan langsung mengubah arah menjadi turun kebawah
            Timer7.Enabled = False
            Timer5.Enabled = False
            Timer6.Enabled = False
            Exit Sub
        End If
        '****************************************** 
        If maze(BADman1ROW, BADman1COLUMN - 1) = "1" And maze(BADman1ROW - 1, BADman1COLUMN) = "1" Then
            Randomize()
            direct = Int(Rnd() * 2) + 1
            If direct = 1 Then
                Timer8.Enabled = True  'sudut kiri atas, bisa turun kekiri atau turun kekanan
                Timer6.Enabled = False
            End If
            If direct = 2 Then     'sudut kiri atas, bisa turun atau kanan, ke kanan
                Timer6.Enabled = True
                Timer8.Enabled = False
            End If
            Timer7.Enabled = False
            Timer5.Enabled = False
            Exit Sub
        End If
        If maze(BADman1ROW, BADman1COLUMN + 1) = "1" And maze(BADman1ROW - 1, BADman1COLUMN) = "1" Then
            Randomize()
            direct = Int(Rnd() * 2) + 1
            If direct = 1 Then
                Timer5.Enabled = True  'sudut kanan atas, bisa turun ke kiri
                Timer8.Enabled = False
            End If
            If direct = 2 Then
                Timer5.Enabled = False 'sudut kanan atas, bisa turun ke kiri
                Timer8.Enabled = True
            End If
            Timer7.Enabled = False
            Timer6.Enabled = False
            Exit Sub
        End If
        If maze(BADman1ROW, BADman1COLUMN - 1) = "1" And maze(BADman1ROW + 1, BADman1COLUMN) = "1" Then
            Randomize()
            direct = Int(Rnd() * 2) + 1
            If direct = 1 Then
                Timer6.Enabled = True
                Timer7.Enabled = False
            End If
            If direct = 2 Then
                Timer7.Enabled = True
                Timer6.Enabled = False
            End If
            Timer8.Enabled = False  'sudut kiri bawah, bisa naik atau mengubah arah kanan jadi ke kanan
            Timer5.Enabled = False
            Exit Sub
        End If
        If maze(BADman1ROW, BADman1COLUMN + 1) = "1" And maze(BADman1ROW + 1, BADman1COLUMN) = "1" Then
            Randomize()
            direct = Int(Rnd() * 2) + 1   '!!!!!!!!!!!!masuk sini sebelum tersesat
            If direct = 1 Then
                Timer5.Enabled = True
                Timer7.Enabled = False
            End If
            If direct = 2 Then
                Timer7.Enabled = True
                Timer5.Enabled = False
            End If
            Timer8.Enabled = False  'sudut kanan bawah, bisa naik ke kiri, jadi arah ke kiri
            Timer6.Enabled = False
            Exit Sub
        End If
        '****************************************** 
        If eatbad = 0 Then
            If BadMan1.Left Mod 32 = 0 And y1 < 0 Then
                If maze(BADman1ROW - 1, BADman1COLUMN) <> "1" Then
                    Timer8.Enabled = False  'diatas terbuka, maka birdman bisa berjalan naik
                    Timer7.Enabled = True
                    Timer5.Enabled = False
                    Timer6.Enabled = False
                    Exit Sub
                End If
            End If
            If BadMan1.Left Mod 32 = 0 And y1 > 0 And maze(BADman1ROW + 1, BADman1COLUMN) <> "1" Then
                Timer7.Enabled = False   'dibawah terbuka, maka birdman bisa berjalan turun
                Timer8.Enabled = True
                Timer5.Enabled = False
                Timer6.Enabled = False
                Exit Sub
            End If
            If BadMan1.Top Mod 32 = 0 And x1 > 0 And maze(BADman1ROW, BADman1COLUMN + 1) <> "1" Then
                Timer7.Enabled = False   'jika sisi kanan terbuka, maka birdman bisa berjalan ke arah kanan
                Timer8.Enabled = False
                Timer5.Enabled = False
                Timer6.Enabled = True
                Exit Sub
            End If
            If BadMan1.Top Mod 32 = 0 And x1 < 0 And maze(BADman1ROW, BADman1COLUMN - 1) <> "1" Then
                Timer7.Enabled = False   'jika sisi kiri terbuka, maka birdman bisa berjalan ke arah kiri
                Timer8.Enabled = False
                Timer5.Enabled = True
                Timer6.Enabled = False
                Exit Sub
            End If
        End If
        '******************************************************
        If eatbad = 1 Then
            If BadMan1.Left Mod 32 = 0 And y1 < 0 Then
                If maze(BADman1ROW + 1, BADman1COLUMN) <> "1" Then
                    Timer8.Enabled = True  'jika birdman berada dibawah, maka BADman akan mengikutinya kebawah
                    Timer7.Enabled = False
                    Timer5.Enabled = False
                    Timer6.Enabled = False
                    Exit Sub
                End If
            End If
            If BadMan1.Left Mod 32 = 0 And y1 > 0 And maze(BADman1ROW - 1, BADman1COLUMN) <> "1" Then
                Timer7.Enabled = True   'jika birdman berada diatas, maka BADman akan mengikutinya keatas
                Timer8.Enabled = False
                Timer5.Enabled = False
                Timer6.Enabled = False
                Exit Sub
            End If
            If BadMan1.Top Mod 32 = 0 And x1 > 0 And maze(BADman1ROW, BADman1COLUMN - 1) <> "1" Then
                Timer7.Enabled = False   'jika birdman berada dikiri, maka BADman akan mengikutinya kekiri
                Timer8.Enabled = False
                Timer5.Enabled = True
                Timer6.Enabled = False
                Exit Sub
            End If
            If BadMan1.Top Mod 32 = 0 And x1 < 0 And maze(BADman1ROW, BADman1COLUMN + 1) <> "1" Then
                Timer7.Enabled = False   'jika birdman berada dikanan, maka BADman akan mengikutinya kekanan
                Timer8.Enabled = False
                Timer5.Enabled = False
                Timer6.Enabled = True
                Exit Sub
            End If
        End If
    End Sub
    Sub badguys2control()
        Dim direct As Integer
        y2 = PacMan.Top - BadMan2.Top   'untuk melihat seberapa jauh jarak atas-bawah BADman dari Birdman
        x2 = PacMan.Left - BadMan2.Left  'untuk melihat seberapa jauh jarak kiri-kanan BADman dari Birdman
        '*******************
        If maze(BADMAN2ROW + 1, BADMAN2COLUMN) = "1" And maze(BADMAN2ROW - 1, BADMAN2COLUMN) = "1" And maze(BADMAN2ROW, BADMAN2COLUMN + 1) = "1" Then 'dead end above
            Timer10.Enabled = False  'buntu ke kanan, BAdman akan berjalan ke kiri
            Timer11.Enabled = False
            Timer9.Enabled = True
            Timer12.Enabled = False
            Exit Sub
        End If
        If maze(BADMAN2ROW + 1, BADMAN2COLUMN) = "1" And maze(BADMAN2ROW - 1, BADMAN2COLUMN) = "1" And maze(BADMAN2ROW, BADMAN2COLUMN - 1) = "1" Then 'dead end above
            Timer9.Enabled = False  'buntu ke kiri, BAdman akan berjalan ke kanan
            Timer11.Enabled = False
            Timer12.Enabled = False
            Timer10.Enabled = True
            Exit Sub
        End If
        If maze(BADMAN2ROW, BADMAN2COLUMN + 1) = "1" And maze(BADMAN2ROW, BADMAN2COLUMN - 1) = "1" And maze(BADMAN2ROW + 1, BADMAN2COLUMN) = "1" Then 'dead end above
            Timer9.Enabled = False  'buntu ke bawah, BAdman akan berjalan ke atas
            Timer11.Enabled = True
            Timer10.Enabled = False
            Timer12.Enabled = False
            Exit Sub
        End If
        If maze(BADMAN2ROW, BADMAN2COLUMN + 1) = "1" And maze(BADMAN2ROW, BADMAN2COLUMN - 1) = "1" And maze(BADMAN2ROW - 1, BADMAN2COLUMN) = "1" Then 'dead end above
            Timer12.Enabled = True  'buntu ke atas, BAdman akan berjalan ke bawah
            Timer9.Enabled = False
            Timer10.Enabled = False
            Timer11.Enabled = False
            Exit Sub
        End If
        '****************************************** 
        If maze(BADMAN2ROW, BADMAN2COLUMN - 1) = "1" And maze(BADMAN2ROW - 1, BADMAN2COLUMN) = "1" Then
            Randomize()
            direct = Int(Rnd() * 2) + 1
            If direct = 1 Then
                Timer12.Enabled = True  'sudut kiri atas, bisa turun atau kanan
                Timer10.Enabled = False
            End If
            If direct = 2 Then     'sudut kiri atas, bisa turun atau kanan
                Timer10.Enabled = True
                Timer12.Enabled = False
            End If
            Timer9.Enabled = False
            Timer11.Enabled = False
            Exit Sub
        End If
        If maze(BADMAN2ROW, BADMAN2COLUMN + 1) = "1" And maze(BADMAN2ROW - 1, BADMAN2COLUMN) = "1" Then
            Randomize()
            direct = Int(Rnd() * 2) + 1
            If direct = 1 Then
                Timer9.Enabled = True  'sudut ke kanan atas, bisa turun atau ke kiri 
                Timer12.Enabled = False
            End If
            If direct = 2 Then
                Timer9.Enabled = False 'sudut ke kanan atas, bisa turun atau kiri 
                Timer12.Enabled = True
            End If
            Timer10.Enabled = False
            Timer11.Enabled = False
            Exit Sub
        End If
        If maze(BADMAN2ROW, BADMAN2COLUMN - 1) = "1" And maze(BADMAN2ROW + 1, BADMAN2COLUMN) = "1" Then
            Randomize()
            direct = Int(Rnd() * 2) + 1
            If direct = 1 Then
                Timer10.Enabled = True
                Timer11.Enabled = False
            End If
            If direct = 2 Then
                Timer11.Enabled = True
                Timer10.Enabled = False
            End If
            Timer12.Enabled = False  'sudut kiri bawah, bisa naik atau ke kanan
            Timer9.Enabled = False
            Exit Sub
        End If
        If maze(BADMAN2ROW, BADMAN2COLUMN + 1) = "1" And maze(BADMAN2ROW + 1, BADMAN2COLUMN) = "1" Then
            Randomize()
            direct = Int(Rnd() * 2) + 1   '!!!!!!!!!!!!masuk kesini sebelum tersesat
            If direct = 1 Then
                Timer9.Enabled = True
                Timer11.Enabled = False
            End If
            If direct = 2 Then
                Timer11.Enabled = True
                Timer9.Enabled = False
            End If
            Timer12.Enabled = False  'sudut ke kanan bawah, bisa naik atau ke kiri
            Timer10.Enabled = False
            Exit Sub
        End If
        '****************************************** 
        If eatbad = 0 Then
            If BadMan2.Left Mod 32 = 0 And y2 < 0 Then
                If maze(BADMAN2ROW - 1, BADMAN2COLUMN) <> "1" Then
                    Timer12.Enabled = False  'atas terbuka, birdman di atas akan berjalan naik
                    Timer11.Enabled = True
                    Timer9.Enabled = False
                    Timer10.Enabled = False
                    Exit Sub
                End If
            End If
            If BadMan2.Left Mod 32 = 0 And y2 > 0 And maze(BADMAN2ROW + 1, BADMAN2COLUMN) <> "1" Then
                Timer11.Enabled = False   'bawah terbuka, birdman dibawah, maka BADman akan turun
                Timer12.Enabled = True
                Timer9.Enabled = False
                Timer10.Enabled = False
                Exit Sub
            End If
            If BadMan2.Top Mod 32 = 0 And x2 > 0 And maze(BADMAN2ROW, BADMAN2COLUMN + 1) <> "1" Then
                Timer11.Enabled = False   'kanan terbuka, birdman di kanan, maka BADman akan berjalan kekanan
                Timer12.Enabled = False
                Timer9.Enabled = False
                Timer10.Enabled = True
                Exit Sub
            End If
            If BadMan2.Top Mod 32 = 0 And x2 < 0 And maze(BADMAN2ROW, BADMAN2COLUMN - 1) <> "1" Then
                Timer11.Enabled = False   'kiri terbuka, birdman di kiri, maka BADman akan berjalan kekiri
                Timer12.Enabled = False
                Timer9.Enabled = True
                Timer10.Enabled = False
                Exit Sub
            End If
        End If
        '************************************
        If eatbad = 1 Then
            If BadMan2.Left Mod 32 = 0 And y2 < 0 Then
                If maze(BADMAN2ROW + 1, BADMAN2COLUMN) <> "1" Then
                    Timer12.Enabled = True  'bawah terbuka, birdman akan berjalan turun
                    Timer11.Enabled = False
                    Timer9.Enabled = False
                    Timer10.Enabled = False
                    Exit Sub
                End If
            End If
            If BadMan2.Left Mod 32 = 0 And y2 > 0 And maze(BADMAN2ROW - 1, BADMAN2COLUMN) <> "1" Then
                Timer11.Enabled = True  'atas terbuka, birdman akan berjalan naik
                Timer12.Enabled = False
                Timer9.Enabled = False
                Timer10.Enabled = False
                Exit Sub
            End If
            If BadMan2.Top Mod 32 = 0 And x2 > 0 And maze(BADMAN2ROW, BADMAN2COLUMN - 1) <> "1" Then
                Timer11.Enabled = False   'kiri terbuka, birdman akan berjalan kekiri
                Timer12.Enabled = False
                Timer9.Enabled = True
                Timer10.Enabled = False
                Exit Sub
            End If
            If BadMan2.Top Mod 32 = 0 And x2 < 0 And maze(BADMAN2ROW, BADMAN2COLUMN + 1) <> "1" Then
                Timer11.Enabled = False   'kanan terbuka, birdman akan berjalan kekanan
                Timer12.Enabled = False
                Timer9.Enabled = False
                Timer10.Enabled = True
                Exit Sub
            End If
        End If
    End Sub

    Sub reset()
        PacMan.Top = 32
        PacMan.Left = 32
        BadMan1.Top = 192
        BadMan1.Left = 256
        BadMan2.Top = 192
        BadMan2.Left = 288
        BadMan1.Image = badman1a.Image
        BadMan2.Image = badman2a.Image
        PacMan.Image = PacClosedRight.Image
        x1 = 0
        y1 = 0
        x2 = 0
        y2 = 0
        y = 0
        win = 0
        score = 0
        BADman1COUNT = 0 'diperlukan agar BADman keluar dari start dan mulai memangsa birdman dalam labirin/maze
    End Sub

    Sub TAKETHEMOUT()
        Timer13.Enabled = True
    End Sub

    Sub TurningSub()
        If WhichKey = 40 Or 38 Then
            If PacMan.Left Mod 32 = 0 Then
                turning = True
                Exit Sub
            End If
        End If
        If WhichKey = 37 Or 39 Then
            If PacMan.Top Mod 32 = 0 Then
                turning = True
                Exit Sub
            End If
        End If

        If WhichKey = 37 Then
            If maze(PacManRow, PacManColumn - 1) = "1" Then
                ShouldExit = True
                Exit Sub
            Else
                turning = True
                Exit Sub
            End If
        End If
        If WhichKey = 39 Then
            If maze(PacManRow, PacManColumn + 1) = "1" Then
                ShouldExit = True
                Exit Sub
            Else
                turning = True
                Exit Sub
            End If
        End If
        If WhichKey = 40 Then
            If maze(PacManRow + 1, PacManColumn) = "1" Then
                ShouldExit = True
                Exit Sub
            Else
                turning = True
                Exit Sub
            End If
        End If
        If WhichKey = 38 Then
            If maze(PacManRow - 1, PacManColumn) = "1" Then
                ShouldExit = True
                Exit Sub
            Else
                turning = True
                Exit Sub
            End If
        End If

    End Sub



    Private Sub eatGhosts_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles eatGhosts.Tick
        'menangani saat hantu sedang makan
        Static count
        count = count + 1
        If count = 1 Then
            y1 = PacMan.Top + 16 - (BadMan1.Top + 16) 'untuk melihat seberapa jauh jarak atas-bawah birdman dari badman_1
            x1 = PacMan.Left + 16 - (BadMan1.Left + 16) 'untuk melihat seberapa jauh jarak kanan-kiri birdman dari badman_1
            If Math.Abs(x1) < Math.Abs(y1) Then

            End If
            If Timer5.Enabled = True And maze(BADman1ROW, BADman1COLUMN + 1) <> "1" Then
                Timer6.Enabled = 1
                Timer7.Enabled = 0
                Timer8.Enabled = 0
                Timer5.Enabled = 0
            End If
            If Timer6.Enabled = True And maze(BADman1ROW, BADman1COLUMN - 1) <> "1" Then
                Timer6.Enabled = 0
                Timer7.Enabled = 0
                Timer8.Enabled = 0
                Timer5.Enabled = 1
            End If
            If Timer7.Enabled = True And maze(BADman1ROW + 1, BADman1COLUMN) <> "1" Then
                Timer8.Enabled = 1
                Timer6.Enabled = 0
                Timer7.Enabled = 0
                Timer5.Enabled = 0
            End If
            If Timer8.Enabled = True And maze(BADman1ROW - 1, BADman1COLUMN) <> "1" Then
                Timer8.Enabled = 0
                Timer6.Enabled = 0
                Timer7.Enabled = 1
                Timer5.Enabled = 0
            End If
            If Timer9.Enabled = True And maze(BADMAN2ROW, BADMAN2COLUMN + 1) <> "1" Then
                Timer10.Enabled = 1
                Timer11.Enabled = 0
                Timer12.Enabled = 0
                Timer9.Enabled = 0
            End If
            If Timer10.Enabled = True And maze(BADMAN2ROW, BADMAN2COLUMN - 1) <> "1" Then
                Timer10.Enabled = 0
                Timer11.Enabled = 0
                Timer12.Enabled = 0
                Timer9.Enabled = 1
            End If
            If Timer11.Enabled = True And maze(BADMAN2ROW + 1, BADMAN2COLUMN) <> "1" Then
                Timer10.Enabled = 0
                Timer11.Enabled = 0
                Timer12.Enabled = 1
                Timer9.Enabled = 0
            End If
            If Timer12.Enabled = True And maze(BADMAN2ROW - 1, BADMAN2COLUMN) <> "1" Then
                Timer10.Enabled = 0
                Timer11.Enabled = 1
                Timer12.Enabled = 0
                Timer9.Enabled = 0
            End If

        End If
       
        If count <= 100 Then
            eatbad = 1
            eatbad = 1
        End If
        If count > 100 Then
            'Label1.Text = ""
            eatbad = 0
            count = 0
            eatGhosts.Enabled = False
        End If

    End Sub

    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        'birdman berjalan ke kiri
        Static x As Integer    'berjalan kekanan

        If Val(bead(PacManRow, PacManColumn)) <> 0 Then
            If maze(PacManRow, PacManColumn) = "3" Then
                eatbad = 1
                Beep()
                maze(PacManRow, PacManColumn) = "2"
                eatGhosts.Enabled = True
            End If
            bigdots(Val(bead(PacManRow, PacManColumn))).Visible = False
            score = score + 1
            bead(PacManRow, PacManColumn) = 0
        End If
        If BADman1COUNT = 0 Then TAKETHEMOUT()
        If maze(PacManRow, PacManColumn + 1) = "1" And PacMan.Left Mod 32 = 0 Then
            Timer1.Enabled = False
            Exit Sub
        End If
        y1 = PacMan.Top + 16 - (BadMan1.Top + 16) 'untuk melihat seberapa jauh jarak atas-bawah birdman dari badman_1
        x1 = PacMan.Left + 16 - (BadMan1.Left + 16) 'untuk melihat seberapa jauh jarak kiri-kanan birdman dari badman_1
        y2 = PacMan.Top + 16 - (BadMan2.Top + 16)
        x2 = PacMan.Left + 16 - (BadMan2.Left + 16)
        If (Math.Abs(x1) <= 16 And Math.Abs(y1) <= 16) Or (Math.Abs(x2) <= 16 And Math.Abs(y2) <= 16) And eatbad = 0 Then
            pacdie.Enabled = True
            Exit Sub
        End If
        PacMan.Left = PacMan.Left + 8
        PacManColumn = ((PacMan.Left + 16) \ 32) + 1   'membuat kolom labirin/maze asli

        If x = 0 Then PacMan.Image = PacClosedRight.Image
        If x = 2 Then PacMan.Image = pacOpenRight.Image

        If turning = True Then
            If WhichKey = 37 Then
                Timer1.Enabled = False
                Timer2.Enabled = True
                turning = False
            End If
            If WhichKey = 39 Then turning = False 'kanan
            If WhichKey = 40 Then
                If PacMan.Left Mod 32 = 0 Then
                    Timer1.Enabled = False
                    Timer3.Enabled = True
                    turning = False
                End If
            End If
            If WhichKey = 38 Then
                If PacMan.Left Mod 32 = 0 Then
                    Timer1.Enabled = False
                    Timer4.Enabled = True
                    turning = False
                End If
            End If
        End If
        x = x + 1
        If x = 3 Then x = -1

       
        If first = 1 And score = 87 Then win = 1
        If first = 2 And score = 90 Then win = 1
        If first = 3 And score = 108 Then win = 1
        If win = 1 Then
            pacdie.Enabled = True
            Beep()
            Exit Sub
        End If
    End Sub

    Private Sub Timer9_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer9.Tick
        'badman_2 pindah kekanan
        Static x As Integer
        x = x + 1
        If x = 3 Then x = -1
        If eatbad = 0 Then
            If x = 0 Then BadMan2.Image = badman2a.Image
            If x = 2 Then BadMan2.Image = badman2b.Image
            y2 = PacMan.Top + 16 - (BadMan2.Top + 16)   'untuk melihat seberapa jauh jarak atas-bawah birdman dari badman_2
            x2 = PacMan.Left + 16 - (BadMan2.Left + 16)  'untuk melihat seberapa jauh jarak kiri-kanan birdman dari badman_2
            If Math.Abs(x2) <= 16 And Math.Abs(y2) <= 16 Then
                pacdie.Enabled = True

                Exit Sub
            End If
        End If
        If eatbad = 1 Then
            If x = 0 Then BadMan2.Image = badman2c.Image
            If x = 2 Then BadMan2.Image = badman2d.Image
            y2 = PacMan.Top + 16 - (BadMan2.Top + 16)   'untuk melihat seberapa jauh jarak atas-bawah birdman dari badman_2
            x2 = PacMan.Left + 16 - (BadMan2.Left + 16)  'untuk melihat seberapa jauh jarak kanan-kiri birdman dari badman_2
            If Math.Abs(x2) <= 16 And Math.Abs(y2) <= 16 Then

                bad2eaten()
                Exit Sub
            End If
        End If
        If maze(BADMAN2ROW, BADMAN2COLUMN - 1) = "1" And BadMan2.Left Mod 32 = 0 Then
            Timer9.Enabled = False  'berhenti berjalan kekiri
            badguys2control()          'memilih arah baru
            Exit Sub
        End If
        BadMan2.Left = BadMan2.Left - 8
        BADMAN2COLUMN = ((BadMan2.Left + 16) \ 32) + 1
        '******************************
        If eatbad = 0 Then
            If BadMan2.Left Mod 32 = 0 Then
                If y2 <= 0 And maze(BADMAN2ROW - 1, BADMAN2COLUMN) <> "1" Then  'birdman berjalan keatas
                    Timer11.Enabled = True    'naik
                    Timer9.Enabled = False
                    Timer10.Enabled = False
                    Timer12.Enabled = False
                    Exit Sub
                End If
                If y2 >= 0 And maze(BADMAN2ROW + 1, BADMAN2COLUMN) <> "1" Then 'pergi ke bawah
                    Timer12.Enabled = True   'turun
                    Timer9.Enabled = False
                    Timer10.Enabled = False
                    Timer11.Enabled = False
                    Exit Sub
                End If
            End If
        End If
        '****************************
        If eatbad = 1 Then
            If BadMan2.Left Mod 32 = 0 Then
                If y2 <= 0 And maze(BADMAN2ROW + 1, BADMAN2COLUMN) <> "1" Then  'berjalan kebawah
                    Timer11.Enabled = False    'turun
                    Timer9.Enabled = False
                    Timer10.Enabled = False
                    Timer12.Enabled = True
                    Exit Sub
                End If
                If y2 >= 0 And maze(BADMAN2ROW - 1, BADMAN2COLUMN) <> "1" Then 'goes right
                    Timer12.Enabled = False   'naik
                    Timer9.Enabled = False
                    Timer10.Enabled = False
                    Timer11.Enabled = True
                    Exit Sub
                End If
            End If
        End If
    End Sub

    Private Sub Timer7_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer7.Tick
        'badman_1 pindah
        Static x As Integer


        x = x + 1
        If x = 3 Then x = -1
        If eatbad = 0 Then
            If x = 0 Then BadMan1.Image = badman1a.Image
            If x = 2 Then BadMan1.Image = badman1b.Image
            y1 = PacMan.Top + 16 - (BadMan1.Top + 16)
            x1 = PacMan.Left + 16 - (BadMan1.Left + 16)
            If Math.Abs(x1) <= 16 And Math.Abs(y1) <= 16 Then
                pacdie.Enabled = True
                Exit Sub
            End If
        End If
        If eatbad = 1 Then
            If x = 0 Then BadMan1.Image = badman1c.Image
            If x = 2 Then BadMan1.Image = badman1d.Image
            y1 = PacMan.Top + 16 - (BadMan1.Top + 16)
            x1 = PacMan.Left + 16 - (BadMan1.Left + 16)
            If Math.Abs(x1) <= 16 And Math.Abs(y1) <= 16 Then

                bad1eaten()
                Exit Sub
            End If
        End If
        If maze(BADman1ROW - 1, BADman1COLUMN) = "1" And BadMan1.Top Mod 32 = 0 Then
            Timer7.Enabled = False   'berhenti naik
            BADGUYS1CONTROL()          'memilih arah baru
            Exit Sub
        End If
        BadMan1.Top = BadMan1.Top - 8
        BADman1ROW = ((BadMan1.Top + 16) \ 32) + 1
        '*****************************
        If eatbad = 0 Then
            If BadMan1.Top Mod 32 = 0 Then
                If x1 <= 0 And maze(BADman1ROW, BADman1COLUMN - 1) <> "1" Then
                    Timer5.Enabled = True  'ke arah kiri
                    Timer7.Enabled = False
                    Timer6.Enabled = False
                    Timer8.Enabled = False
                    Exit Sub
                End If
                If x1 >= 0 And maze(BADman1ROW, BADman1COLUMN + 1) <> "1" Then
                    Timer6.Enabled = True   'ke arah kanan
                    Timer7.Enabled = False
                    Timer8.Enabled = False
                    Timer5.Enabled = False
                    Exit Sub
                End If
            End If
        End If
        '*********************************************
        If eatbad = 1 Then
            If BadMan1.Top Mod 32 = 0 Then
                If x1 <= 0 And maze(BADman1ROW, BADman1COLUMN + 1) <> "1" Then
                    Timer5.Enabled = False 'dari kiri, birdman berjalan kekanan
                    Timer7.Enabled = False
                    Timer6.Enabled = True
                    Timer8.Enabled = False
                    Exit Sub
                End If
                If x1 >= 0 And maze(BADman1ROW, BADman1COLUMN - 1) <> "1" Then  'goes right
                    Timer6.Enabled = False   'dari kanan, birdman berjalan kekiri
                    Timer7.Enabled = False
                    Timer8.Enabled = False
                    Timer5.Enabled = True
                    Exit Sub
                End If
            End If
        End If
    End Sub

    Private Sub pacdie_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles pacdie.Tick
        'penanganan jika birdman mati
        Static x As Integer
        If x = 0 Or win = 1 Then
            PacMan.Image = PacBlack0.Image
            Timer1.Enabled = False
            Timer2.Enabled = False
            Timer3.Enabled = False
            Timer4.Enabled = False
            Timer5.Enabled = False
            Timer6.Enabled = False
            Timer7.Enabled = False
            Timer8.Enabled = False
            Timer9.Enabled = False
            Timer10.Enabled = False
            Timer11.Enabled = False
            Timer12.Enabled = False
        End If
        If x = 2 And win <> 1 Then PacMan.Image = PacBlack1.Image
        If x = 4 And win <> 1 Then PacMan.Image = PacBlack2.Image
        If x = 6 And win <> 1 Then PacMan.Image = PacBlack3.Image
        If x = 8 And win <> 1 Then PacMan.Image = PacBlack4.Image
        If x = 10 And win <> 1 Then PacMan.Image = PacBlack5.Image
        If x = 12 And win <> 1 Then PacMan.Image = PacBlack6.Image
        If x = 14 And win <> 1 Then PacMan.Image = PacBlack7.Image
        If x = 16 And win <> 1 Then PacMan.Image = PacBlack8.Image
        If x = 18 And win <> 1 Then PacMan.Image = PacBlack9.Image
        x = x + 1
        If x = 20 Then
            Form2.Visible = True
            Form2.Label5.Visible = True
            If win <> 1 Then
                Form2.Label5.Text = "You Lose"
                AxWindowsMediaPlayer1.URL = "Pacman-Death.mid"
                badscore = badscore + 1
            End If
            If win = 1 Then
                pacscore = pacscore + 1

                Form2.Label5.Text = "You Win"
                AxWindowsMediaPlayer1.URL = "Pacman-Death.mid"
            End If
        End If
        If x = 30 Then
            Form2.Visible = False
            pacdie.Enabled = False

            reset()
            loadMaze()
            x = 0
        End If

    End Sub

    Private Sub Timer8_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer8.Tick
        'badman_1 pergi kebawah
        Static x As Integer
        x = x + 1
        If x = 3 Then x = -1
        If eatbad = 0 Then
            If x = 0 Then BadMan1.Image = badman1a.Image
            If x = 2 Then BadMan1.Image = badman1b.Image
            y1 = PacMan.Top + 16 - (BadMan1.Top + 16)
            x1 = PacMan.Left + 16 - (BadMan1.Left + 16)
            If Math.Abs(x1) <= 16 And Math.Abs(y1) <= 16 Then
                pacdie.Enabled = True
                Exit Sub
            End If
        End If
        If eatbad = 1 Then
            If x = 0 Then BadMan1.Image = badman1c.Image
            If x = 2 Then BadMan1.Image = badman1d.Image
            y1 = PacMan.Top + 16 - (BadMan1.Top + 16)   'untuk melihat jarak atas-bawah birdman dari badman_1
            x1 = PacMan.Left + 16 - (BadMan1.Left + 16)  'untuk melihat jarak kanan-kiri birdman dari badman_1
            If Math.Abs(x1) <= 16 And Math.Abs(y1) <= 16 Then
                'pacdie.Enabled = True
                bad1eaten()
                Exit Sub
            End If
        End If
        If maze(BADman1ROW + 1, BADman1COLUMN) = "1" And BadMan1.Top Mod 32 = 0 Then
            Timer8.Enabled = False   'pacman berhenti jika menabrak dinding labirin/maze
            BADGUYS1CONTROL()
            Exit Sub
        End If
        BadMan1.Top = BadMan1.Top + 8
        BADman1ROW = ((BadMan1.Top + 16) \ 32) + 1
        '***********************
        If eatbad = 0 Then
            If BadMan1.Top Mod 32 = 0 Then
                If x1 <= 0 And maze(BADman1ROW, BADman1COLUMN - 1) <> "1" Then
                    Timer5.Enabled = True   'ke kiri
                    Timer8.Enabled = False
                    Timer7.Enabled = False
                    Timer6.Enabled = False
                    Exit Sub
                End If
                If x1 >= 0 And maze(BADman1ROW, BADman1COLUMN + 1) <> "1" Then
                    Timer6.Enabled = True     'ke kanan
                    Timer8.Enabled = False
                    Timer5.Enabled = False
                    Timer7.Enabled = False
                    Exit Sub
                End If
            End If
        End If
        '**************************
        If eatbad = 1 Then
            If BadMan1.Top Mod 32 = 0 Then
                If x1 <= 0 And maze(BADman1ROW, BADman1COLUMN + 1) <> "1" Then
                    Timer5.Enabled = False   'ke kanan
                    Timer8.Enabled = False
                    Timer7.Enabled = False
                    Timer6.Enabled = True
                    Exit Sub
                End If
                If x1 >= 0 And maze(BADman1ROW, BADman1COLUMN - 1) <> "1" Then
                    Timer6.Enabled = False     'kekiri
                    Timer8.Enabled = False
                    Timer5.Enabled = True
                    Timer7.Enabled = False
                    Exit Sub
                End If
            End If
        End If
    End Sub

    Private Sub Timer6_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer6.Tick
        'badman_1 berjalan kekanan
        Static x As Integer
        'label1 = "Timer6"
        x = x + 1
        If x = 3 Then x = -1
        If eatbad = 0 Then
            If x = 0 Then BadMan1.Image = badman1a.Image
            If x = 2 Then BadMan1.Image = badman1b.Image
            If Math.Abs(x1) <= 16 And Math.Abs(y1) <= 16 Then
                pacdie.Enabled = True
                Exit Sub
            End If
        End If
        If eatbad = 1 Then
            If x = 0 Then BadMan1.Image = badman1c.Image
            If x = 2 Then BadMan1.Image = badman1d.Image
            If Math.Abs(x1) <= 16 And Math.Abs(y1) <= 16 Then
                'pacdie.Enabled = True
                bad1eaten()
                Exit Sub
            End If
        End If
        y1 = PacMan.Top + 16 - (BadMan1.Top + 16)
        x1 = PacMan.Left + 16 - (BadMan1.Left + 16)
        If BADman1COUNT = 0 Then TAKETHEMOUT()
        If maze(BADman1ROW, BADman1COLUMN + 1) = "1" And BadMan1.Left Mod 32 = 0 Then  'jika blok selanjutnya
            Timer6.Enabled = False        'adalah satu jalur, maka waktu berhenti badman_1 tidak dapat berjalan lurus
            BADGUYS1CONTROL()     'berhenti berjalan ke kanan dan mulai mencari arah baru
            Exit Sub
        End If
        BadMan1.Left = BadMan1.Left + 8
        BADman1COLUMN = ((BadMan1.Left + 16) \ 32) + 1   'memberi kolom pada labirin
        If BadMan1.Left Mod 32 = 0 Then
            'Label2 = "in"
            If y1 <= 0 And maze(BADman1ROW + 1, BADman1COLUMN) <> "1" Then
                Timer7.Enabled = False     'ke bawah
                Timer6.Enabled = False
                Timer5.Enabled = False
                Timer8.Enabled = True
                Exit Sub
            End If
            If y1 >= 0 And maze(BADman1ROW - 1, BADman1COLUMN) <> "1" Then 'goes right
                Timer8.Enabled = False     'ke atas
                Timer6.Enabled = True
                Timer5.Enabled = False
                Timer7.Enabled = True
                Exit Sub
            End If
        End If
    End Sub
    Sub bad1eaten()
        BadMan1.Top = 256
        BadMan1.Left = 256
        Dim dir As Integer
        dir = Int(Rnd() * 2) + 1
        If dir = 1 Then
            Timer5.Enabled = True
            Timer6.Enabled = False
            Timer7.Enabled = False
            Timer8.Enabled = False
        End If
        If dir = 2 Then
            Timer5.Enabled = False
            Timer6.Enabled = True
            Timer7.Enabled = False
            Timer8.Enabled = False
        End If
        BADman1COLUMN = ((BadMan1.Left + 16) \ 32) + 1
        BADman1ROW = ((BadMan1.Top + 16) \ 32) + 1
    End Sub
    Sub bad2eaten()
        BadMan2.Top = 256
        BadMan2.Left = 288
        Dim dir As Integer
        dir = Int(Rnd() * 2) + 1
        If dir = 1 Then
            Timer9.Enabled = True
            Timer10.Enabled = False
            Timer11.Enabled = False
            Timer12.Enabled = False
        End If
        If dir = 2 Then
            Timer9.Enabled = False
            Timer10.Enabled = True
            Timer11.Enabled = False
            Timer12.Enabled = False
        End If
        BADMAN2COLUMN = ((BadMan2.Left + 16) \ 32) + 1
        BADMAN2ROW = ((BadMan2.Top + 16) \ 32) + 1
    End Sub

    Private Sub Timer5_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer5.Tick
        'badman1 moves left
        Static x As Integer
        x = x + 1
        If x = 3 Then x = -1
        If eatbad = 0 Then
            If x = 0 Then BadMan1.Image = badman1a.Image
            If x = 2 Then BadMan1.Image = badman1b.Image
            y1 = PacMan.Top + 16 - (BadMan1.Top + 16) 'untuk melihat seberapa jauh jarak atass-bawah badman_1 dengan pacman
            x1 = PacMan.Left + 16 - (BadMan1.Left + 16)
            If Math.Abs(x1) <= 16 And Math.Abs(y1) <= 16 Then
                pacdie.Enabled = True
                Exit Sub
            End If
        End If
        If eatbad = 1 Then
            If x = 0 Then BadMan1.Image = badman1c.Image
            If x = 2 Then BadMan1.Image = badman1d.Image
            y1 = PacMan.Top + 16 - (BadMan1.Top + 16)
            x1 = PacMan.Left + 16 - (BadMan1.Left + 16) 'untuk melihat seberapa jauh jarak kiri-kanan badman_1 dengan pacman
            If Math.Abs(x1) <= 16 And Math.Abs(y1) <= 16 Then
                'pacdie.Enabled = True\
                bad1eaten()
                Exit Sub
            End If
        End If
        If maze(BADman1ROW, BADman1COLUMN - 1) = "1" And BadMan1.Left Mod 32 = 0 Then
            Timer5.Enabled = False  'berhenti berjalan ke kiri
            BADGUYS1CONTROL()          'cari arah baru
            Exit Sub
        End If
        BadMan1.Left = BadMan1.Left - 8
        BADman1COLUMN = ((BadMan1.Left + 16) \ 32) + 1
        If BadMan1.Left Mod 32 = 0 Then
            If y1 <= 0 And maze(BADman1ROW + 1, BADman1COLUMN) <> "1" Then
                Timer7.Enabled = False    'pergi kebawah
                Timer5.Enabled = False
                Timer6.Enabled = False
                Timer8.Enabled = True
                Exit Sub
            End If
            If y1 >= 0 And maze(BADman1ROW - 1, BADman1COLUMN) <> "1" Then 'goes right
                Timer8.Enabled = False  'pergi keatas
                Timer5.Enabled = False
                Timer6.Enabled = False
                Timer7.Enabled = True
                Exit Sub
            End If
        End If
    End Sub

    Private Sub Timer4_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer4.Tick

        Static x As Integer
        'Label1.Text = Str$(PacManRow) + " " + Str$(PacManColumn)
        If Val(bead(PacManRow, PacManColumn)) <> 0 Then
            If maze(PacManRow, PacManColumn) = "3" Then
                eatbad = 1
                Beep()
                maze(PacManRow, PacManColumn) = "2"
                eatGhosts.Enabled = True
            End If
           
            bigdots(Val(bead(PacManRow, PacManColumn))).Visible = 0

            score = score + 1
            bead(PacManRow, PacManColumn) = 0
        End If
        If BADman1COUNT = 0 Then TAKETHEMOUT()

        If maze(PacManRow - 1, PacManColumn) = "1" And PacMan.Top Mod 32 = 0 Then
            Timer4.Enabled = False  'pergi keatas
            Exit Sub
        End If
        y1 = PacMan.Top + 16 - (BadMan1.Top + 16)
        x1 = PacMan.Left + 16 - (BadMan1.Left + 16)
        y2 = PacMan.Top + 16 - (BadMan2.Top + 16)
        x2 = PacMan.Left + 16 - (BadMan2.Left + 16)
        If (Math.Abs(x1) <= 16 And Math.Abs(y1) <= 16) Or (Math.Abs(x2) <= 16 And Math.Abs(y2) <= 16) And eatbad = 0 Then
            pacdie.Enabled = True
            Exit Sub
        End If
        PacMan.Top = PacMan.Top - 8
        PacManRow = ((PacMan.Top + 16) \ 32) + 1
        'label1 = PacManRow & "," & PacManColumn
        If x = 0 Then PacMan.Image = PacClosedUp.Image
        If x = 2 Then PacMan.Image = PacOpenUp.Image

        If turning = True Then
            If WhichKey = 37 Then
                If PacMan.Top Mod 32 = 0 Then
                    Timer4.Enabled = False
                    Timer2.Enabled = True
                    turning = False
                End If
            End If
            If WhichKey = 39 Then
                If PacMan.Top Mod 32 = 0 Then
                    Timer4.Enabled = False
                    Timer1.Enabled = True
                    turning = False
                End If
            End If
            If WhichKey = 40 Then
                Timer4.Enabled = False
                Timer3.Enabled = True
                turning = False
            End If
            If WhichKey = 38 Then turning = False
        End If
        'label1 = Maze(PacManRow, PacManColumn)
        x = x + 1
        If x = 3 Then x = -1

        'label1 = score
        If first = 1 And score = 87 Then win = 1
        If first = 2 And score = 90 Then win = 1
        If first = 3 And score = 108 Then win = 1
        If win = 1 Then
            pacdie.Enabled = True
            Beep()
            Exit Sub
        End If

    End Sub

    Private Sub Timer3_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer3.Tick
        'birdman berjalan kebawah
        Static x As Integer
        'Label1.Text = Str$(PacManRow) + " " + Str$(PacManColumn)
        If Val(bead(PacManRow, PacManColumn)) <> 0 Then
            If maze(PacManRow, PacManColumn) = "3" Then
                eatbad = 1
                Beep()
                maze(PacManRow, PacManColumn) = "2"
                eatGhosts.Enabled = True
            End If
            bigdots(Val(bead(PacManRow, PacManColumn))).Visible = 0
            'ImgBigDot(PacManrow, PacManColumn).Visible = 0 'wipes the beads
            score = score + 1
            bead(PacManRow, PacManColumn) = 0
        End If
        If BADman1COUNT = 0 Then TAKETHEMOUT()
        If maze(PacManRow + 1, PacManColumn) = "1" And PacMan.Top Mod 32 = 0 Then
            Timer3.Enabled = False   'goes down
            Exit Sub
        End If
        y1 = PacMan.Top + 16 - (BadMan1.Top + 16) 'untuk melihat seberapa jauh jarak atas-bawah birdman dengan badman_1
        x1 = PacMan.Left + 16 - (BadMan1.Left + 16) 'untuk melihat seberapa jauh jarak kanan-kiri birdman dengan badman_1
        y2 = PacMan.Top + 16 - (BadMan2.Top + 16)
        x2 = PacMan.Left + 16 - (BadMan2.Left + 16)
        'label1 = score
        If (Math.Abs(x1) <= 16 And Math.Abs(y1) <= 16) Or (Math.Abs(x2) <= 16 And Math.Abs(y2) <= 16) And eatbad = 0 Then
            pacdie.Enabled = True
            Exit Sub
        End If
        PacMan.Top = PacMan.Top + 8
        PacManRow = ((PacMan.Top + 16) \ 32) + 1
        'label1 = PacManRow & "," & PacManColumn
        If x = 0 Then PacMan.Image = pacClosedDown.Image
        If x = 2 Then PacMan.Image = PacOpenDown.Image

        If turning = True Then
            If WhichKey = 37 Then
                If PacMan.Top Mod 32 = 0 Then
                    Timer3.Enabled = False
                    Timer2.Enabled = True
                    turning = False
                End If
            End If
            If WhichKey = 39 Then
                If PacMan.Top Mod 32 = 0 Then
                    Timer3.Enabled = False
                    Timer1.Enabled = True
                    turning = False
                End If
            End If
            If WhichKey = 40 Then turning = False
            If WhichKey = 38 Then
                Timer3.Enabled = False
                Timer4.Enabled = True
                turning = False
            End If
        End If
        'label1 = Maze(PacManRow, PacManColumn)
        x = x + 1
        If x = 3 Then x = -1


        'label1 = score
        If first = 1 And score = 87 Then win = 1
        If first = 2 And score = 90 Then win = 1
        If first = 3 And score = 108 Then win = 1
        If win = 1 Then
            pacdie.Enabled = True
            Beep()
            Exit Sub
        End If

    End Sub

    Private Sub Timer2_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer2.Tick
        'birdman berjalan kekiri
        Static x As Integer
        'Label1.Text = Str$(PacManRow) + " " + Str$(PacManColumn)
        If Val(bead(PacManRow, PacManColumn)) <> 0 Then
            If maze(PacManRow, PacManColumn) = "3" Then
                eatbad = 1
                Beep()
                maze(PacManRow, PacManColumn) = "2"
                eatGhosts.Enabled = True
            End If
            bigdots(Val(bead(PacManRow, PacManColumn))).Visible = 0
            score = score + 1
            bead(PacManRow, PacManColumn) = 0
        End If
        If BADman1COUNT = 0 Then TAKETHEMOUT()
        If maze(PacManRow, PacManColumn - 1) = "1" And PacMan.Left Mod 32 = 0 Then
            Timer2.Enabled = False  'berjalan kearah kiri
            Exit Sub
        End If
        y1 = PacMan.Top + 16 - (BadMan1.Top + 16)
        x1 = PacMan.Left + 16 - (BadMan1.Left + 16)
        y2 = PacMan.Top + 16 - (BadMan2.Top + 16)
        x2 = PacMan.Left + 16 - (BadMan2.Left + 16)
        If (Math.Abs(x1) <= 16 And Math.Abs(y1) <= 16) Or (Math.Abs(x2) <= 16 And Math.Abs(y2) <= 16) And eatbad = 0 Then
            pacdie.Enabled = True
            Exit Sub
        End If
        PacMan.Left = PacMan.Left - 8
        PacManColumn = ((PacMan.Left + 16) \ 32) + 1
        'label1 = PacManRow & "," & PacManColumn
        If x = 0 Then PacMan.Image = pacClosedLeft.Image
        If x = 2 Then PacMan.Image = PacOpenLeft.Image

        If turning = True Then
            If WhichKey = 37 Then turning = False
            If WhichKey = 39 Then
                Timer2.Enabled = False
                Timer1.Enabled = True
                turning = False
            End If
            If WhichKey = 40 Then
                If PacMan.Left Mod 32 = 0 Then
                    Timer2.Enabled = False
                    Timer3.Enabled = True
                    turning = False
                End If
            End If
            If WhichKey = 38 Then
                If PacMan.Left Mod 32 = 0 Then
                    Timer2.Enabled = False
                    Timer4.Enabled = True
                    turning = False
                End If
            End If
            'label1 = Maze(PacManRow, PacManColumn)
        End If

        x = x + 1
        If x = 3 Then x = -1

        'label1 = score

        If first = 1 And score = 87 Then win = 1
        If first = 2 And score = 90 Then win = 1
        If first = 3 And score = 108 Then win = 1
        If win = 1 Then
            pacdie.Enabled = True
            Beep()
            Exit Sub
        End If

    End Sub

    Private Sub Timer10_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer10.Tick
        'badman_2 berjalan kekanan
        Static x As Integer    'goes right
        'label1 = "Timer6"
        x = x + 1
        If x = 3 Then x = -1
        If eatbad = 0 Then
            If x = 0 Then BadMan2.Image = badman2a.Image
            If x = 2 Then BadMan2.Image = badman2b.Image
            y2 = PacMan.Top + 16 - (BadMan2.Top + 16)  'untuk melihat seberapa jauh jarak atas-bawah birdman dengan badman_2
            x2 = PacMan.Left + 16 - (BadMan2.Left + 16)   'untuk melihat seberapa jauh jarak kiri-kanan birdman dengan badman_2
            If Math.Abs(x2) <= 16 And Math.Abs(y2) <= 16 Then
                pacdie.Enabled = True
                Exit Sub
            End If
        End If
        If eatbad = 1 Then
            If x = 0 Then BadMan2.Image = badman2c.Image
            If x = 2 Then BadMan2.Image = badman2d.Image
            y2 = PacMan.Top + 16 - (BadMan2.Top + 16)
            x2 = PacMan.Left + 16 - (BadMan2.Left + 16)
            If Math.Abs(x2) <= 16 And Math.Abs(y2) <= 16 Then
                'pacdie.Enabled = True
                bad2eaten()
                Exit Sub
            End If
        End If
        If maze(BADMAN2ROW, BADMAN2COLUMN + 1) = "1" And BadMan2.Left Mod 32 = 0 Then
            Timer10.Enabled = False
            badguys2control()     'berhenti berjalan kekanan dan mulai mencari arah baru
            Exit Sub
        End If
        BadMan2.Left = BadMan2.Left + 8
        BADMAN2COLUMN = ((BadMan2.Left + 16) \ 32) + 1
        '***********************************
        If eatbad = 0 Then
            If BadMan2.Left Mod 32 = 0 Then
                'Label2 = "in"
                If y2 <= 0 And maze(BADMAN2ROW - 1, BADMAN2COLUMN) <> "1" Then
                    Timer11.Enabled = True     'keatas
                    Timer10.Enabled = False
                    Timer9.Enabled = False
                    Timer12.Enabled = False
                    Exit Sub
                End If
                If y2 >= 0 And maze(BADMAN2ROW + 1, BADMAN2COLUMN) <> "1" Then 'kekanan
                    Timer12.Enabled = True     'kebawah
                    Timer10.Enabled = False
                    Timer9.Enabled = False
                    Timer11.Enabled = False
                    Exit Sub
                End If
            End If
        End If
        '********************
        If eatbad = 1 Then
            If BadMan2.Left Mod 32 = 0 Then
                'Label2 = "in"
                If y2 <= 0 And maze(BADMAN2ROW + 1, BADMAN2COLUMN) <> "1" Then
                    Timer11.Enabled = False     'kebawah
                    Timer10.Enabled = False
                    Timer9.Enabled = False
                    Timer12.Enabled = True
                    Exit Sub
                End If
                If y2 >= 0 And maze(BADMAN2ROW - 1, BADMAN2COLUMN) <> "1" Then
                    Timer12.Enabled = False     'keatas
                    Timer10.Enabled = False
                    Timer9.Enabled = False
                    Timer11.Enabled = True
                    Exit Sub
                End If
            End If
        End If

    End Sub

    Private Sub Timer13_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer13.Tick
        'handles get the bad guys out of the starting box
        Static x As Integer
      
        Static count
        'Label1.Text = count
        If x = 0 Then
            BadMan1.Image = badman1a.Image
            BadMan2.Image = badman2a.Image
        End If
        If x = 2 Then
            BadMan1.Image = badman1b.Image
            BadMan2.Image = badman2b.Image
        End If
        x = x + 1
        If x = 3 Then x = -1
        count = count + 1
        BADman1COUNT = BADman1COUNT + 1
        If count <= 12 And BadMan1.Top <= 250 Then 'badman_1 turun
            pict(178).Visible = 0  'opens gate
            BadMan1.Top = BadMan1.Top + 8
            BADman1ROW = ((BadMan1.Top + 16) \ 32) + 1
        End If
        If BadMan1.Top = 256 Then      'badman_1 turun dan belok kanan
            BadMan1.Left = BadMan1.Left + 8
            BADman1COLUMN = ((BadMan1.Left + 16) \ 32) + 1
        End If
        If BadMan1.Left = 384 Then       'muncul di sudut kanan bawah kotak
            BadMan1.Top = BadMan1.Top - 8
            BADman1ROW = ((BadMan1.Top + 16) \ 32) + 1
        End If
        If count > 49 Then '39 Then      'badman_1 turun dan belok kanan
            BadMan1.Left = BadMan1.Left + 8
            BADman1COLUMN = ((BadMan1.Left + 16) \ 32) + 1
        End If
        If BadMan2.Left >= 261 And count > 24 Then 'badman_2 berjalan kekiri
            BadMan2.Left = BadMan2.Left - 8
            BADMAN2COLUMN = ((BadMan2.Left + 16) \ 32) + 1
        End If
        If BadMan2.Left = 256 Then   'badman_2 pergi keluar pintu
            BadMan2.Top = BadMan2.Top + 8
            BADMAN2ROW = ((BadMan2.Top + 16) \ 32) + 1
        End If
        If BadMan2.Top = 256 Then  'badman_2 pergi kekiri
            pict(178).Visible = True   'pintu tertutup kembali
            BadMan2.Left = BadMan2.Left - 8
            BADMAN2COLUMN = ((BadMan2.Left + 16) \ 32) + 1
        End If
        If count = 44 Then
            Timer13.Enabled = 0
            BADGUYS1CONTROL()
            count = 0
            badguys2control()
        End If

    End Sub

    Private Sub Timer12_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer12.Tick
        'badman_2 pergi kebawah
        Static x As Integer
        x = x + 1
        If x = 3 Then x = -1
        If eatbad = 0 Then
            If x = 0 Then BadMan2.Image = badman2a.Image
            If x = 2 Then BadMan2.Image = badman2b.Image
            y2 = PacMan.Top + 16 - (BadMan2.Top + 16)  'untuk melihat seberapa jauh jarak atas-bawah badman_2 dari birdman
            x2 = PacMan.Left + 16 - (BadMan2.Left + 16)   'untuk melihat seberapa jauh jarak kiri-kanan badman_2 dari birdman
            If Math.Abs(x2) <= 16 And Math.Abs(y2) <= 16 Then
                pacdie.Enabled = True
                Exit Sub
            End If
        End If
        If eatbad = 1 Then
            If x = 0 Then BadMan2.Image = badman2c.Image
            If x = 2 Then BadMan2.Image = badman2d.Image
            y2 = PacMan.Top + 16 - (BadMan2.Top + 16)  'untuk melihat seberapa jauh jarak atas-bawah badman_2 dari birdman
            x2 = PacMan.Left + 16 - (BadMan2.Left + 16)   'untuk melihat seberapa jauh jarak kiri-kanan badman_2 dari birdman
            If Math.Abs(x2) <= 16 And Math.Abs(y2) <= 16 Then
                'pacdie.Enabled = True
                bad2eaten()
                Exit Sub
            End If
        End If
        If maze(BADMAN2ROW + 1, BADMAN2COLUMN) = "1" And BadMan2.Top Mod 32 = 0 Then  'mematikan disini
            Timer12.Enabled = False   'birdman akan berhenti saat dia menabrak dinding dan akan berjalan turun
            badguys2control()
            Exit Sub
        End If
        BadMan2.Top = BadMan2.Top + 8
        BADMAN2ROW = ((BadMan2.Top + 16) \ 32) + 1
        '******************
        If eatbad = 0 Then
            If BadMan2.Top Mod 32 = 0 Then
                If x2 <= 0 And maze(BADMAN2ROW, BADMAN2COLUMN - 1) <> "1" Then
                    Timer9.Enabled = True   'ke kiri
                    Timer12.Enabled = False
                    Timer11.Enabled = False
                    Timer10.Enabled = False
                    Exit Sub
                End If
                If x2 >= 0 And maze(BADMAN2ROW, BADMAN2COLUMN + 1) <> "1" Then
                    Timer10.Enabled = True     'ke kanan
                    Timer12.Enabled = False
                    Timer9.Enabled = False
                    Timer11.Enabled = False
                    Exit Sub
                End If
            End If
        End If
        '************************
        If eatbad = 1 Then
            If BadMan2.Top Mod 32 = 0 Then
                If x2 <= 0 And maze(BADMAN2ROW, BADMAN2COLUMN + 1) <> "1" Then
                    Timer9.Enabled = False   'ke kanan
                    Timer12.Enabled = False
                    Timer11.Enabled = False
                    Timer10.Enabled = True
                    Exit Sub
                End If
                If x2 >= 0 And maze(BADMAN2ROW, BADMAN2COLUMN - 1) <> "1" Then
                    Timer10.Enabled = True     'kekiri
                    Timer12.Enabled = False
                    Timer9.Enabled = False
                    Timer11.Enabled = False
                    Exit Sub
                End If
            End If
        End If

    End Sub

    Private Sub Timer11_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer11.Tick

        Static x As Integer

        x = x + 1
        If x = 3 Then x = -1
        If eatbad = 0 Then
            If x = 0 Then BadMan2.Image = badman2a.Image
            If x = 2 Then BadMan2.Image = badman2b.Image
            y2 = PacMan.Top + 16 - (BadMan2.Top + 16) 'untuk melihat seberapa jauh jarak atas-bawah badman_2 dari birdman
            x2 = PacMan.Left + 16 - (BadMan2.Left + 16)  'untuk melihat seberapa jauh jarak kiri-kanan badman_2 dari birdman
            If Math.Abs(x2) <= 16 And Math.Abs(y2) <= 16 Then
                pacdie.Enabled = True
                Exit Sub
            End If
        End If
        If eatbad = 1 Then
            If x = 0 Then BadMan2.Image = badman2c.Image
            If x = 2 Then BadMan2.Image = badman2d.Image
            y2 = PacMan.Top + 16 - (BadMan2.Top + 16) 'untuk melihat seberapa jauh jarak atas-bawah badman_2 dari birdman
            x2 = PacMan.Left + 16 - (BadMan2.Left + 16)  'untuk melihat seberapa jauh jarak kiri-kanan badman_2 dari birdman
            If Math.Abs(x2) <= 16 And Math.Abs(y2) <= 16 Then
                'pacdie.Enabled = True
                bad2eaten()
                Exit Sub
            End If
        End If
        If maze(BADMAN2ROW - 1, BADMAN2COLUMN) = "1" And BadMan2.Top Mod 32 = 0 Then
            Timer11.Enabled = False  'berhenti berjalan keatas
            badguys2control()          'cari arah baru
            Exit Sub
        End If
        BadMan2.Top = BadMan2.Top - 8
        BADMAN2ROW = ((BadMan2.Top + 16) \ 32) + 1

        If eatbad = 0 Then
            If BadMan2.Top Mod 32 = 0 Then
                If x2 <= 0 And maze(BADMAN2ROW, BADMAN2COLUMN - 1) <> "1" Then
                    Timer9.Enabled = True  'ke kiri
                    Timer11.Enabled = False
                    Timer10.Enabled = False
                    Timer12.Enabled = False
                    Exit Sub
                End If
                If x2 >= 0 And maze(BADMAN2ROW, BADMAN2COLUMN + 1) <> "1" Then
                    Timer10.Enabled = True   'ke kanan
                    Timer11.Enabled = False
                    Timer12.Enabled = False
                    Timer9.Enabled = False
                    Exit Sub
                End If
            End If
        End If
        '****************************
        If eatbad = 1 Then
            If BadMan2.Top Mod 32 = 0 Then
                If x2 <= 0 And maze(BADMAN2ROW, BADMAN2COLUMN + 1) <> "1" Then
                    Timer9.Enabled = False  'ke kanan
                    Timer11.Enabled = False
                    Timer10.Enabled = True
                    Timer12.Enabled = False
                    Exit Sub
                End If
                If x2 >= 0 And maze(BADMAN2ROW, BADMAN2COLUMN - 1) <> "1" Then
                    Timer10.Enabled = False  'ke kiri
                    Timer11.Enabled = False
                    Timer12.Enabled = False
                    Timer9.Enabled = True
                    Exit Sub
                End If
            End If
        End If
    End Sub

End Class
