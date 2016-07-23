using System.Collections.Generic;
using Rezoom.ADO.Materialization;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Rezoom.ADO.Test.Internals
{
    [TestClass]
    public class TestMaterialization
    {
        public class ConstructorPoint
        {
            public ConstructorPoint(int x, int y)
            {
                X = x;
                Y = y;
            }

            public int X { get; }
            public int Y { get; }
        }

        [TestMethod]
        public void TestSimpleConstructor()
        {
            var template = RowReaderTemplate<ConstructorPoint>.Template;
            var reader = template.CreateReader();
            reader.ProcessColumnMap(ColumnMap.Parse(new[] { "X", "Y" }));
            reader.ProcessRow(new object[] { 3, 5 });
            var point = reader.ToEntity();
            Assert.AreEqual(3, point.X);
            Assert.AreEqual(5, point.Y);
        }

        public class Point { public int X { get; set; } public int Y { get; set; } }
        [TestMethod]
        public void TestSimplePropertyAssignment()
        {
            var template = RowReaderTemplate<Point>.Template;
            var reader = template.CreateReader();
            reader.ProcessColumnMap(ColumnMap.Parse(new[] { "X", "Y" }));
            reader.ProcessRow(new object[] { 3, 5 });
            var point = reader.ToEntity();
            Assert.AreEqual(3, point.X);
            Assert.AreEqual(5, point.Y);
        }

        public class User
        {
            public int Id { get; set; }
            public string Name { get; set; }
            public Group[] Groups { get; set; }

            public class Group
            {
                public int Id { get; set; }
                public string Name { get; set; }
            }
        }

        [TestMethod]
        public void TestArrayNavProperty()
        {
            var template = RowReaderTemplate<User>.Template;
            var reader = template.CreateReader();
            var columnMap = ColumnMap.Parse(new[] { "Id", "Name", "Groups$Id", "Name" });
            reader.ProcessColumnMap(columnMap);
            reader.ProcessRow(new object[] { 1, "bob", 2, "developers" });
            reader.ProcessRow(new object[] { 1, "bob", 3, "testers" });
            var user = reader.ToEntity();
            Assert.AreEqual(1, user.Id);
            Assert.AreEqual("bob", user.Name);
            Assert.AreEqual(2, user.Groups.Length);
            Assert.AreEqual(2, user.Groups[0].Id);
            Assert.AreEqual("developers", user.Groups[0].Name);
            Assert.AreEqual(3, user.Groups[1].Id);
            Assert.AreEqual("testers", user.Groups[1].Name);
        }

        [TestMethod]
        public void TestManyArrayNavProperty()
        {
            var template = RowReaderTemplate<User[]>.Template;
            var reader = template.CreateReader();
            var columnMap = ColumnMap.Parse(new[] { "Id", "Name", "Groups$Id", "Name" });
            reader.ProcessColumnMap(columnMap);
            reader.ProcessRow(new object[] { 1, "bob", 2, "developers" });
            reader.ProcessRow(new object[] { 1, "bob", 3, "testers" });
            reader.ProcessRow(new object[] { 2, "jim", 2, "developers" });
            reader.ProcessRow(new object[] { 2, "jim", 4, "slackers" });
            var users = reader.ToEntity();
            Assert.AreEqual(2, users.Length);

            Assert.AreEqual(1, users[0].Id);
            Assert.AreEqual("bob", users[0].Name);
            Assert.AreEqual(2, users[0].Groups.Length);
            Assert.AreEqual(2, users[0].Groups[0].Id);
            Assert.AreEqual("developers", users[0].Groups[0].Name);
            Assert.AreEqual(3, users[0].Groups[1].Id);
            Assert.AreEqual("testers", users[0].Groups[1].Name);

            Assert.AreEqual(2, users[1].Id);
            Assert.AreEqual("jim", users[1].Name);
            Assert.AreEqual(2, users[1].Groups.Length);
            Assert.AreEqual(2, users[1].Groups[0].Id);
            Assert.AreEqual("developers", users[1].Groups[0].Name);
            Assert.AreEqual(4, users[1].Groups[1].Id);
            Assert.AreEqual("slackers", users[1].Groups[1].Name);
        }

        public class NestUser
        {
            public int Id { get; set; }
            public string Name { get; set; }
            public Group[] Groups { get; set; }

            public class Group
            {
                public int Id { get; set; }
                public string Name { get; set; }
                public Tag[] Tags { get; set; }
            }
            public class Tag
            {
                public int Id { get; set; }
                public string Name { get; set; }
            }
        }

        [TestMethod]
        public void TestNestedArrayNavProperty()
        {
            var template = RowReaderTemplate<NestUser>.Template;
            var reader = template.CreateReader();
            var columnMap = ColumnMap.Parse(new[] { "Id", "Name", "Groups$Id", "Name", "Groups$Tags$Id", "Name" });
            reader.ProcessColumnMap(columnMap);
            reader.ProcessRow(new object[] { 1, "bob", 2, "developers", 4, "t1" });
            reader.ProcessRow(new object[] { 1, "bob", 2, "developers", 5, "t2" });
            reader.ProcessRow(new object[] { 1, "bob", 2, "developers", 6, "t3" });
            reader.ProcessRow(new object[] { 1, "bob", 3, "testers", 4, "t1" });
            reader.ProcessRow(new object[] { 1, "bob", 3, "testers", 5, "t2" });
            reader.ProcessRow(new object[] { 1, "bob", 3, "testers", 7, "t4" });
            var user = reader.ToEntity();
            Assert.AreEqual(1, user.Id);
            Assert.AreEqual("bob", user.Name);
            Assert.AreEqual(2, user.Groups.Length);
            Assert.AreEqual(2, user.Groups[0].Id);
            Assert.AreEqual("developers", user.Groups[0].Name);
            Assert.AreEqual(3, user.Groups[0].Tags.Length);
            Assert.AreEqual("t2", user.Groups[0].Tags[1].Name);
            Assert.AreEqual(3, user.Groups[1].Id);
            Assert.AreEqual("testers", user.Groups[1].Name);
            Assert.AreEqual(3, user.Groups[1].Tags.Length);
            Assert.AreEqual("t4", user.Groups[1].Tags[2].Name);
        }

        public class Folder
        {
            public int Id { get; set; }
            public string Name { get; set; }
            public List<Folder> Children { get; set; }
        }

        [TestMethod]
        public void TestRecursiveArrayNavProperties()
        {
            var template = RowReaderTemplate<Folder>.Template;
            var reader = template.CreateReader();
            var columnMap = ColumnMap.Parse(new[] { "Id", "Name", "Children$Id", "Name", "Children$Children$Id", "Name" });
            reader.ProcessColumnMap(columnMap);
            reader.ProcessRow(new object[] { 1, "f1", 2, "f1.1", 3, "f1.1.1" });
            reader.ProcessRow(new object[] { 1, "f1", 4, "f1.2", 5, "f1.2.1" });
            var folder = reader.ToEntity();
            Assert.AreEqual(1, folder.Id);
            Assert.AreEqual("f1", folder.Name);

            Assert.AreEqual(2, folder.Children[0].Id);
            Assert.AreEqual("f1.1", folder.Children[0].Name);
            Assert.AreEqual(3, folder.Children[0].Children[0].Id);
            Assert.AreEqual("f1.1.1", folder.Children[0].Children[0].Name);

            Assert.AreEqual(4, folder.Children[1].Id);
            Assert.AreEqual("f1.2", folder.Children[1].Name);
            Assert.AreEqual(5, folder.Children[1].Children[0].Id);
            Assert.AreEqual("f1.2.1", folder.Children[1].Children[0].Name);
        }

        public class Zoo
        {
            public int Id { get; set; }
            public string Name { get; set; }
        }
        public class Animal
        {
            public Zoo Zoo { get; set; }
            public int Id { get; set; }
            public string Name { get; set; }
        }

        [TestMethod]
        public void TestSingleNavProperty()
        {
            var template = RowReaderTemplate<Animal>.Template;
            var reader = template.CreateReader();
            var columnMap = ColumnMap.Parse(new[] { "Id", "Name", "Zoo$Id", "Name" });
            reader.ProcessColumnMap(columnMap);
            reader.ProcessRow(new object[] { 1, "zebra", 2, "NC zoo" });
            var animal = reader.ToEntity();
            Assert.AreEqual(1, animal.Id);
            Assert.AreEqual("zebra", animal.Name);
            Assert.IsNotNull(animal.Zoo);
            Assert.AreEqual(2, animal.Zoo.Id);
            Assert.AreEqual("NC zoo", animal.Zoo.Name);
        }

        [TestMethod]
        public void TestSingleNullNavProperty()
        {
            var template = RowReaderTemplate<Animal>.Template;
            var reader = template.CreateReader();
            var columnMap = ColumnMap.Parse(new[] { "Id", "Name", "Zoo$Id", "Name" });
            reader.ProcessColumnMap(columnMap);
            reader.ProcessRow(new object[] { 1, "zebra", null, null });
            var animal = reader.ToEntity();
            Assert.AreEqual(1, animal.Id);
            Assert.AreEqual("zebra", animal.Name);
            Assert.IsNull(animal.Zoo);
        }
    }
}
