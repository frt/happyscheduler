import { DebugElement } from '@angular/core';
import { TestBed, async, inject } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpModule, Http } from '@angular/http';
import { InfoMessagesService } from '../info-messages.service';

import { Observable} from 'rxjs/Observable';
import 'rxjs/add/observable/of';

import { ComponentFixtureAutoDetect } from '@angular/core/testing';
import { By } from '@angular/platform-browser';

import { TaskListComponent } from './task-list.component';

describe('TaskListComponent', () => {
    let fixture;
    let component: TaskListComponent;
    let el;
    let initialNrOfTasks;

    beforeEach(async(() => {
        TestBed.configureTestingModule({
            declarations: [
                TaskListComponent
            ],
            imports: [
                RouterTestingModule,
                HttpModule
            ],
            providers: [
                { provide: ComponentFixtureAutoDetect, useValue: true }
            ]
        }).compileComponents();
        fixture = TestBed.createComponent(TaskListComponent);
        el = fixture.debugElement.nativeElement;

        component = fixture.componentInstance;
        // tasks array example:
        component.tasks = [{
            time: 30,
            dueDate: '2017-08-01',
            done: false,
            name: 'test remove',
            id: 1,
            happy: true
        }, {
            time: 31,
            dueDate: '2017-08-03',
            done: false,
            name: 'test done',
            id: 2,
            happy: true
        }];
        initialNrOfTasks = component.tasks.length;
    }));

    it('should not have an <hr> element',  async(() => {
        expect(el.querySelectorAll('hr').length).toEqual(0);
    }));

    it('should have a delete button', async(() => {
        expect(el.querySelectorAll('button#delete-task-1.glyphicon-remove').length)
            .toBeGreaterThan(0);
    }));

    it('should have a done button', async(() => {
        expect(el.querySelectorAll('button#done-task-2.glyphicon-ok').length)
            .toBeGreaterThan(0);
    }));

    it('should have a due date column', async(() => {
        // second column header
        expect(el.querySelector('table.table thead tr td:nth-child(2)').textContent)
            .toEqual('due date');

        // first line, second column
        expect(el.querySelector('table.table tbody tr td:nth-child(2)').textContent)
            .toContain('2017-08-01');
    }));


    it('should delete a task when delete button is clicked', async(() => {
        const http = fixture.debugElement.injector.get(Http);
        spyOn(http, 'delete').and.returnValue(Observable.of('DELETED'));

        expect(el.querySelectorAll('.task-item').length).toEqual(initialNrOfTasks);

        // click on the delete button of the first task
        fixture.debugElement.query(By.css('button#delete-task-1'))
            .triggerEventHandler('click', null);
        fixture.detectChanges();

        expect(http.delete).toHaveBeenCalledWith('../tasks/1');
        expect(el.querySelectorAll('.task-item').length).toEqual(initialNrOfTasks - 1);
    }));

    it('should mark a task as done when done button is clicked', async(() => {
        const http = fixture.debugElement.injector.get(Http);
        spyOn(http, 'put').and.returnValue(Observable.of('UPDATED'));

        expect(el.querySelectorAll('.task-item').length).toEqual(initialNrOfTasks);

        fixture.debugElement.query(By.css('button#done-task-2'))
            .triggerEventHandler('click', null);
        fixture.detectChanges();

        expect(http.put).toHaveBeenCalledWith('../tasks/2',
            '{"time":31,"dueDate":"2017-08-03","done":true,"name":"test done","id":2,"happy":true}');
        expect(el.querySelectorAll('.task-item').length)
            .toEqual(initialNrOfTasks - 1);    // will take the task
                                              // out of the list
                                              // for now
    }));
});
